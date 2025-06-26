module Workflow where

import Control.Exception (throw)
import Control.Monad.IO.Class (liftIO)
import Data.Aeson (FromJSON, ToJSON)
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.Encoding (decodeUtf8)
import Data.Time.Clock (getCurrentTime, UTCTime)
import GHC.Generics (Generic)
import RequireCallStack (RequireCallStack, provideCallStack)
import Temporal.Activity (Activity)
import Temporal.Duration (seconds)
import Temporal.Exception (ApplicationFailure (..))
import Temporal.Payload (JSON (JSON))
import Temporal.TH (WorkflowFn, ActivityFn)
import Temporal.TH qualified
import Temporal.Workflow (Workflow, WorkflowId (..))
import Temporal.Workflow qualified as StartActivityOptions (StartActivityOptions (..))
import Temporal.Workflow qualified as Workflow
import UnliftIO.Exception (throwIO)

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
  deriving stock (Eq, Generic, Show)
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
  billOrder . applyMonthlySpecial $ confirmationFromOrder order currtime
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

    billOrder :: OrderConfirmation -> Activity () OrderConfirmation
    billOrder = validateOrder
      -- | imagine there's some payment processing going on here, too.
      -- hence doing this in Activity () rather than as a pure function
      where
        validateOrder :: OrderConfirmation -> Activity () OrderConfirmation
        validateOrder order@OrderConfirmation{..} =
          -- | imagine more complex validation...
          if ocAmountCents < 0 then reportAmountError ocAmountCents else pure order
        reportAmountError amount = liftIO $ throwIO
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
