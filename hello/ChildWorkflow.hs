{-# LANGUAGE DuplicateRecordFields #-}

module Main where

import Control.Monad.IO.Class (liftIO)
import Control.Monad.Logger (defaultOutput)
import Control.Monad.Trans.Reader (runReaderT)
import Data.Aeson (FromJSON, ToJSON)
import Data.Text (Text)
import Data.UUID qualified as UUID
import Data.UUID.V4 qualified as UUID.V4
import DiscoverInstances (discoverInstances)
import GHC.Generics (Generic)
import RequireCallStack (RequireCallStack, provideCallStack)
import System.IO (stdout)
import Temporal.Activity (Activity)
import Temporal.Client (mkWorkflowClientConfig, workflowClient)
import Temporal.Client qualified as Client
import Temporal.Core.Client (connectClient, defaultClientConfig)
import Temporal.Duration (seconds)
import Temporal.Payload (JSON (JSON))
import Temporal.Runtime (TelemetryOptions (..), initializeRuntime)
import Temporal.TH (ActivityFn, WorkflowFn)
import Temporal.TH qualified
import Temporal.Worker (Worker, WorkerConfig, startWorker)
import Temporal.Worker qualified as Worker
import Temporal.Workflow
  ( StartChildWorkflowOptions (workflowId),
    Workflow,
    WorkflowId (..),
  )
import Temporal.Workflow qualified as Workflow
import UnliftIO.Exception (bracket)

data ComposeGreetingInput = ComposeGreetingInput
  { greeting :: Text,
    name :: Text
  }
  deriving stock (Generic, Show)
  deriving anyclass (FromJSON, ToJSON)

composeGreeting :: ComposeGreetingInput -> Activity () Text
composeGreeting input = do
  pure (input.greeting <> ", " <> input.name <> "!")

Temporal.TH.registerActivity 'composeGreeting
Temporal.TH.bringRegisteredTemporalFunctionsIntoScope

activityOptions :: Workflow.StartActivityOptions
activityOptions =
  Workflow.defaultStartActivityOptions (Workflow.StartToClose $ seconds 1)

composeGreetingWorkflow :: ComposeGreetingInput -> Workflow Text
composeGreetingWorkflow input = provideCallStack do
  Workflow.executeActivity ComposeGreeting activityOptions input

Temporal.TH.registerWorkflow 'composeGreetingWorkflow
Temporal.TH.bringRegisteredTemporalFunctionsIntoScope

-- | As with 'ComposeGreetingInput', use a standalone type for the workflow
-- input so that it can be versioned in a backwards-compatible manner.
newtype GreetingInput = GreetingInput { name :: Text }
  deriving stock (Generic, Show)
  deriving anyclass (FromJSON, ToJSON)

-- | The simplest possible child workflow delegation: spawn a child workflow
-- with default options and wait for it to complete.
greetingWorkflow :: GreetingInput -> Workflow Text
greetingWorkflow (GreetingInput name) = provideCallStack do
  Workflow.executeChildWorkflow
    ComposeGreetingWorkflow
    options
    (ComposeGreetingInput "Hello" name)
  where
    options =
      Workflow.defaultChildWorkflowOptions
        { workflowId = Just "hello-child-workflow-workflow-child-id"
        }

Temporal.TH.registerWorkflow 'greetingWorkflow
Temporal.TH.bringRegisteredTemporalFunctionsIntoScope

taskQueue :: Workflow.TaskQueue
taskQueue = "hello-child-workflow-task-queue"

namespace :: Workflow.Namespace
namespace = "default"

workerConfig :: WorkerConfig ()
workerConfig = provideCallStack $ Worker.configure environment definitions settings
  where
    environment = ()
    definitions :: (RequireCallStack) => Worker.Definitions ()
    definitions = Temporal.TH.discoverDefinitions @() $$(discoverInstances) $$(discoverInstances)
    settings = do
      Worker.setNamespace namespace
      Worker.setTaskQueue taskQueue
      Worker.setLogger (defaultOutput stdout)

main :: IO ()
main = bracket setup teardown $ \(withClient, _) -> do
  workflowId <- WorkflowId . UUID.toText <$> liftIO UUID.V4.nextRandom
  _result <-
    withClient $
      Client.execute
        GreetingWorkflow
        workflowId
        (Client.startWorkflowOptions taskQueue)
        (GreetingInput "World")
  pure ()
  where
    setup = do
      runtime <- initializeRuntime NoTelemetry
      coreClient <- connectClient runtime defaultClientConfig
      worker <- startWorker coreClient workerConfig
      client <- workflowClient coreClient (mkWorkflowClientConfig namespace)
      let withClient action = runReaderT action client
      pure (withClient, worker)

    teardown (_, worker) = Worker.shutdown worker
