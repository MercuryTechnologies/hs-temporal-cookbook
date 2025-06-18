module Main where

import Control.Monad.IO.Class (liftIO)
import Control.Monad.Logger (defaultOutput, runStdoutLoggingT)
import Control.Monad.Trans.Reader (runReaderT)
import Data.UUID qualified as UUID
import Data.UUID.V4 qualified as UUID.V4
import RequireCallStack (RequireCallStack, provideCallStack)
import System.IO (stdout)
import Temporal.Client (mkWorkflowClientConfig, workflowClient)
import Temporal.Client qualified as Client
import Temporal.Core.Client (connectClient, defaultClientConfig)
import Temporal.Runtime (TelemetryOptions (..), initializeRuntime)
import Temporal.TH (WorkflowFn, ActivityFn)
import Temporal.TH qualified
import Temporal.Worker qualified as Worker
import Temporal.Workflow (Workflow, WorkflowId (..))
import Temporal.Workflow qualified as Workflow
import UnliftIO.Exception (bracket)
import Workflow

taskQueue :: Workflow.TaskQueue
taskQueue = "pizza-tasks"

namespace :: Workflow.Namespace
namespace = "default"

main :: IO ()
main = bracket setup teardown $ \withClient -> do
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

      client <- workflowClient coreClient (mkWorkflowClientConfig namespace)
      let withClient action = runReaderT action client

      pure withClient

    teardown _withClient = pure ()

    buildTestPizzaOrder :: PizzaOrder
    buildTestPizzaOrder =
      let address = CustomerAddress "701 Mission Street" "Apartment 9C"
          largeVeg = Pizza "Large, with mushrooms and onions" 1500
          smallPep = Pizza "Small, with pepperoni" 1200
       in PizzaOrder address 31337 [largeVeg, smallPep]
