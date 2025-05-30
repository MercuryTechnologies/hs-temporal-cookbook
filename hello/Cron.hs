module Main where

import Control.Monad.IO.Class (liftIO)
import Control.Monad.Logger (defaultOutput, runStdoutLoggingT)
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
import Temporal.Client.Schedule (mkScheduleAction, mkScheduleClient)
import Temporal.Core.Client (connectClient, defaultClientConfig)
import Temporal.Duration (seconds)
import Temporal.Payload (JSON (JSON))
import Temporal.Runtime (TelemetryOptions (..), initializeRuntime)
import Temporal.TH (ActivityFn, WorkflowFn)
import Temporal.TH qualified
import Temporal.Worker (Worker, WorkerConfig, startWorker)
import Temporal.Worker qualified as Worker
import Temporal.Workflow (Workflow, WorkflowId (..))
import Temporal.Workflow qualified as Workflow
import UnliftIO.Concurrent (threadDelay)
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

greetingWorkflow :: ComposeGreetingInput -> Workflow Text
greetingWorkflow input = provideCallStack do
  Workflow.executeActivity ComposeGreeting activityOptions input

Temporal.TH.registerWorkflow 'greetingWorkflow
Temporal.TH.bringRegisteredTemporalFunctionsIntoScope

taskQueue :: Workflow.TaskQueue
taskQueue = "hello-cron-task-queue"

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
main = bracket setup teardown $ \(withClient, _) -> do
  workflowId <- WorkflowId . UUID.toText <$> liftIO UUID.V4.nextRandom
  -- Use the client to start a scheduled workflow, which will run once per
  -- minute until this program is shut down.
  --
  -- Unlike some of the other simple workflow execution examples, we use
  -- 'Client.start' to get a 'WorkflowHandle' (rather than a concrete result).
  --
  -- Note that, in most production setups, the client & worker(s) would be in
  -- separate processes so that the lifetime of one would not be tied to that
  -- of the other.
  _handle <-
    withClient $
      Client.start
        GreetingWorkflow
        workflowId
        ((Client.startWorkflowOptions taskQueue) {Client.cronSchedule = Just "* * * * *"})
        (ComposeGreetingInput "Hello" "World")
  -- Keep the worker running for 10 minutes, so that at most 10 workflow
  -- executions can be observed in the web UI.
  threadDelay 600_000_000
  where
    setup = do
      runtime <- initializeRuntime NoTelemetry
      coreClient <- runStdoutLoggingT $ connectClient runtime defaultClientConfig
      worker <- startWorker coreClient workerConfig
      client <- workflowClient coreClient (mkWorkflowClientConfig namespace)
      let withClient action = runReaderT action client
      pure (withClient, worker)

    teardown (_, worker) = Worker.shutdown worker
