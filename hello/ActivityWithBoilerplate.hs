module Main where

import Control.Monad.IO.Class (liftIO)
import Control.Monad.Logger (defaultOutput)
import Control.Monad.Trans.Reader (runReaderT)
import Data.Aeson (FromJSON, ToJSON)
import Data.Text (Text)
import Data.UUID qualified as UUID
import Data.UUID.V4 qualified as UUID.V4
import GHC.Generics (Generic)
import RequireCallStack (RequireCallStack, provideCallStack)
import System.IO (stdout)
import Temporal.Activity (Activity, ActivityDefinition, ProvidedActivity, provideActivity)
import Temporal.Client (mkWorkflowClientConfig, workflowClient)
import Temporal.Client qualified as Client
import Temporal.Core.Client (connectClient, defaultClientConfig)
import Temporal.Duration (seconds)
import Temporal.Payload (JSON (JSON))
import Temporal.Runtime (TelemetryOptions (..), initializeRuntime)
import Temporal.TH qualified
import Temporal.Worker (Worker, WorkerConfig, startWorker)
import Temporal.Worker qualified as Worker
import Temporal.Workflow (ProvidedWorkflow, Workflow, WorkflowId (..), provideWorkflow)
import Temporal.Workflow qualified as Workflow
import UnliftIO.Exception (bracket)

-- | Arguments that shall be passed to 'composeGreeting'.
--
-- While we could write an 'Activity' that accepts multiple arguments in the
-- in the same manner as any normal Haskell function, it's strongly encouraged
-- to use standalone types that can have fields added to them in a
-- backwards-compatible fashion.
data ComposeGreetingInput = ComposeGreetingInput
  { greeting :: Text,
    name :: Text
  }
  deriving stock (Generic, Show)
  deriving anyclass (FromJSON, ToJSON)

-- | Basic 'Activity' that does string concatenation.
composeGreeting :: ComposeGreetingInput -> Activity () Text
composeGreeting input = do
  pure (input.greeting <> ", " <> input.name <> "!")

-- | Construct a 'ProvidedActivity' by associating 'composeGreeting' with a
-- codec (in this case, 'JSON') and a name (@"greeting"@).
--
-- This produces a composite type that keeps the argument(s), codec, and result
-- types in sync so that changes to the function are reflected at use-sites.
greetingActivity :: ProvidedActivity () (ComposeGreetingInput -> Activity () Text)
greetingActivity = provideActivity JSON "greeting" composeGreeting

-- | Similar to the 'ProvidedActivity', above, construct a 'ProvidedWorkflow'
-- by associating the workflow implementation (defined below) with a codec and
-- a name.
--
-- This is the simplest possible workflow implementation: it executes
-- 'greetingActivity' by passing 'ComposeGreetingInput', with the constraint
-- that execution may last no longer than 1 second from "start to close".
greetingWorkflow :: ProvidedWorkflow (ComposeGreetingInput -> Workflow Text)
greetingWorkflow = provideCallStack $ provideWorkflow JSON "greeting" impl
  where
    impl :: (RequireCallStack) => ComposeGreetingInput -> Workflow Text
    impl input = Workflow.executeActivity greetingActivity options input

    options :: Workflow.StartActivityOptions
    options =
      Workflow.defaultStartActivityOptions (Workflow.StartToClose $ seconds 1)

taskQueue :: Workflow.TaskQueue
taskQueue = "hello-activity-with-boilerplate-task-queue"

namespace :: Workflow.Namespace
namespace = "default"

-- | A 'WorkerConfig' consists of the following components:
-- 
-- * an environment value, which will be passed to any 'Activity' that the
--   'Worker' picks up
-- * a set of 'Worker.Definitions', which map 'Workflow' and 'Activity' names
--   to their implementations
-- * a stateful configuration record (of type 'Worker.ConfigM'), from which
--   'WorkerConfig' fields can be set directly
--
-- It's important to note that 'Activity' execution environment is available
-- when constructing 'Worker.ConfigM'; it's empty ('()') in this example, but
-- in a more involved setting it can be used to share structured logging,
-- instrumentation, etc. configurations.
workerConfig :: WorkerConfig ()
workerConfig = Worker.configure environment definitions settings
  where
    environment = ()
    definitions = (greetingActivity, greetingWorkflow)
    settings = do
      Worker.setNamespace namespace
      Worker.setTaskQueue taskQueue
      Worker.setLogger (defaultOutput stdout)

-- | Main entrypoint, which is responsible for:
--
-- * initializing the @tokio@ runtime and thread-pool
-- * spawning a 'Worker', based on the 'workerConfig' above
-- * constructing a 'workflowClient' that  can execute 'greetingWorkflow's
-- * submitting a 'greetingWorkflow' for execution
-- * performing a graceful teardown upon completion 
--
-- These wouldn't be colocated within the same module in a production setting;
-- rather, one or more persistent 'Worker's would be spawned from a
-- 'WorkerConfig' whose configuration values overlap with any clients that
-- submit jobs for execution.
main :: IO ()
main = bracket setup teardown $ \(withClient, worker) -> do
  workflowId <- WorkflowId . UUID.toText <$> liftIO UUID.V4.nextRandom
  _result <-
    withClient $
      Client.execute
        greetingWorkflow
        workflowId
        (Client.startWorkflowOptions taskQueue)
        (ComposeGreetingInput "Hello" "World")
  pure ()
  where
    setup = do
      runtime <- initializeRuntime NoTelemetry
      coreClient <- connectClient runtime defaultClientConfig

      worker <- startWorker coreClient workerConfig

      -- 'Client.execute' expects a context with @HasWorkflowClient@; for the
      -- sake of example, we can provide that here with a simple wrapper. 
      client <- workflowClient coreClient (mkWorkflowClientConfig namespace)
      let withClient action = runReaderT action client

      pure (withClient, worker)

    teardown (_withClient, worker) = Worker.shutdown worker
