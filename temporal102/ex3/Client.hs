module Main where

import Control.Exception (throw)
import Control.Monad (when)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Logger (defaultOutput, runStdoutLoggingT)
import Control.Monad.Trans.Reader (runReaderT)
import Data.Aeson (FromJSON, ToJSON)
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.Encoding (decodeUtf8)
import Data.Time.Clock (getCurrentTime, UTCTime)
import Data.UUID qualified as UUID
import Data.UUID.V4 qualified as UUID.V4
import DiscoverInstances (discoverInstances)
import GHC.Generics (Generic)
import Network.HTTP.Simple (httpBS, getResponseBody, parseRequest)
import RequireCallStack (RequireCallStack, provideCallStack)
import System.IO (stdout)
import Temporal.Activity (Activity)
import Temporal.Client (mkWorkflowClientConfig, workflowClient)
import Temporal.Client qualified as Client
import Temporal.Core.Client (connectClient, defaultClientConfig)
import Temporal.Duration (seconds)
import Temporal.Exception (ApplicationFailure (..))
import Temporal.Payload (JSON (JSON))
import Temporal.Runtime (TelemetryOptions (..), initializeRuntime)
import Temporal.TH (WorkflowFn, ActivityFn)
import Temporal.TH qualified
import Temporal.Worker (Worker, WorkerConfig, startWorker)
import Temporal.Worker qualified as Worker
import Temporal.Workflow (Workflow, WorkflowId (..))
import Temporal.Workflow qualified as StartActivityOptions (StartActivityOptions (..))
import Temporal.Workflow qualified as Workflow
import UnliftIO.Exception (bracket)

-- | utility we'll need, defining it here to avoid TH splice shenanigans
tshow :: Show a => a -> Text
tshow = Text.pack . show

-- | We'll define two activities: One that mimics a distance
-- calculation, and one that implements a simple bill calculator. Both
-- will have record types for input and output.
data CustomerAddress = CustomerAddress
  { address1 :: Text
  , address2 :: Text
  }
  deriving stock (Generic, Show)
  deriving anyclass (FromJSON, ToJSON)

data CustomerDistance = CustomerDistance
  { kilometers :: Int
  }
  deriving stock (Generic, Show)
  deriving anyclass (FromJSON, ToJSON)

-- | This is an arbitrary, but stable, "distance calculator" for a
-- hypothetical address type. Being in an Activity, a "real" version
-- would have access to IO and would probably call an API somewhere, but
-- this suffices for the example.
getDistance :: CustomerAddress -> Activity () CustomerDistance
getDistance CustomerAddress{..} =
  let dist = (Text.length address1) + (Text.length address2) - 10
      corrDist = if dist < 1 then 5 else dist
   in pure $ CustomerDistance corrDist

Temporal.TH.registerActivity 'getDistance

-- | A small set of domain types for a pizza order
data Pizza = Pizza
  { name :: Text
  , price :: Int
  }
  deriving stock (Generic, Show)
  deriving anyclass (FromJSON, ToJSON)

data PizzaOrder = PizzaOrder
  { poAddress :: CustomerAddress
  , poOrderNumber :: Int
  , poPizzas :: [Pizza]
  }
  deriving stock (Generic, Show)
  deriving anyclass (FromJSON, ToJSON)

data OrderConfirmation = OrderConfirmation
  { ocOrderNumber :: Int
  , ocConfirmationNumber :: Text -- for some reason
  , ocTimestamp :: UTCTime
  , ocAmountCents :: Int
  }
  deriving stock (Generic, Show)
  deriving anyclass (FromJSON, ToJSON)

billCustomer :: PizzaOrder -> Activity () OrderConfirmation
billCustomer order = do
  currtime <- liftIO getCurrentTime
  pure . billOrder . applyMonthlySpecial $ confirmationFromOrder order currtime
  where
    confirmationFromOrder :: PizzaOrder -> UTCTime -> OrderConfirmation
    confirmationFromOrder PizzaOrder{..} currtime =
      OrderConfirmation
        { ocOrderNumber = poOrderNumber
        , ocConfirmationNumber = "AB123CD"
        , ocTimestamp = currtime
        , ocAmountCents = foldr (+) 0 $ fmap price poPizzas
        }
    -- | Monthly special: Orders over $30 get $5 off!
    applyMonthlySpecial :: OrderConfirmation -> OrderConfirmation
    applyMonthlySpecial order@OrderConfirmation{..} =
      if ocAmountCents > 3000 then order { ocAmountCents = -500 } else order

    billOrder :: OrderConfirmation -> OrderConfirmation
    billOrder = validateOrder
      -- | imagine there's some payment processing going on here, too
      where
        validateOrder :: OrderConfirmation -> OrderConfirmation
        validateOrder order@OrderConfirmation{..} =
          if ocAmountCents < 0 then reportAmountError ocAmountCents else order
        reportAmountError amount = throw
          ApplicationFailure
            { type' = "OrderAmountError"
            , message = "Order amount must be nonnegative, got " <> tshow amount
            , nonRetryable = False
            , details = []
            , stack = ""
            , nextRetryDelay = Nothing
            }

Temporal.TH.registerActivity 'billCustomer

retryPolicy :: Workflow.RetryPolicy
retryPolicy =
  Workflow.defaultRetryPolicy { Workflow.maximumInterval = Just $ seconds 10 }

activityOptions :: Workflow.StartActivityOptions
activityOptions =
  (Workflow.defaultStartActivityOptions (Workflow.StartToClose $ seconds 5))
    { StartActivityOptions.retryPolicy = Just retryPolicy }

pizzaWorkflow :: PizzaOrder -> Workflow OrderConfirmation
pizzaWorkflow order@PizzaOrder{..} = provideCallStack do
  distance <- Workflow.executeActivity GetDistance activityOptions order.poAddress
  let validOrder = deliverableOrder order distance
  
  Workflow.sleep $ seconds 3

  Workflow.executeActivity BillCustomer activityOptions validOrder

  where
    deliverableOrder order distance =
      if distance.kilometers > 30 then reportTooDistantCustomer distance else order
    reportTooDistantCustomer distance = throw
      ApplicationFailure
        { type' = "TooDistantCustomerError"
        , message = "Customer must be within 30km, got " <> tshow distance.kilometers <> "km"
        , nonRetryable = True
        , details = []
        , stack = ""
        , nextRetryDelay = Nothing
        }

Temporal.TH.registerWorkflow 'pizzaWorkflow

taskQueue :: Workflow.TaskQueue
taskQueue = "pizza-tasks"

namespace :: Workflow.Namespace
namespace = "default"

workerConfig :: WorkerConfig ()
workerConfig = provideCallStack $ Worker.configure environment definitions settings
  where
    environment = ()
    definitions :: RequireCallStack => Worker.Definitions ()
    definitions = Temporal.TH.discoverDefinitions $$(discoverInstances) $$(discoverInstances)
    settings = do
      Worker.setNamespace namespace
      Worker.setTaskQueue taskQueue
      Worker.setLogger (defaultOutput stdout)

main :: IO ()
main = bracket setup teardown $ \(withClient, worker) -> do
  workflowId <- WorkflowId . UUID.toText <$> liftIO UUID.V4.nextRandom
  _result <-
    withClient $
      Client.execute
        PizzaWorkflow
        workflowId
        (Client.startWorkflowOptions taskQueue)
        buildTestPizzaOrder
  pure ()
  where
    setup = do
      runtime <- initializeRuntime NoTelemetry
      coreClient <- runStdoutLoggingT $ connectClient runtime defaultClientConfig

      worker <- startWorker coreClient workerConfig

      client <- workflowClient coreClient (mkWorkflowClientConfig namespace)
      let withClient action = runReaderT action client

      pure (withClient, worker)

    teardown (_withClient, worker) = Worker.shutdown worker

    buildTestPizzaOrder :: PizzaOrder
    buildTestPizzaOrder =
      let address = CustomerAddress "701 Mission Street" "Apartment 9C"
          largeVeg = Pizza "Large, with mushrooms and onions" 1500
          smallPep = Pizza "Small, with pepperoni" 1200
       in PizzaOrder address 31337 [largeVeg, smallPep]
