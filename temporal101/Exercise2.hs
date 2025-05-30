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
import SayHello
import System.IO (stdout)
import Temporal.Activity (Activity)
import Temporal.Client (mkWorkflowClientConfig, workflowClient)
import Temporal.Client qualified as Client
import Temporal.Core.Client (connectClient, defaultClientConfig)
import Temporal.Duration (seconds)
import Temporal.Payload (JSON (JSON))
import Temporal.Runtime (TelemetryOptions (..), initializeRuntime)
import Temporal.TH (WorkflowFn, ActivityFn)
import Temporal.TH qualified
import Temporal.Worker (Worker, WorkerConfig, startWorker)
import Temporal.Worker qualified as Worker
import Temporal.Workflow (Workflow, WorkflowId (..))
import Temporal.Workflow qualified as Workflow
import UnliftIO.Exception (bracket)

taskQueue :: Workflow.TaskQueue
taskQueue = "hello-worker"

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
--
-- Special attention should be paid to 'Temporal.TH.discoverDefinitions': this
-- macro accepts a type argument for some 'Activity' environment, and two
-- typeclass dictionaries (a list of 'Temporal.TH.WorkflowFn' and
-- 'Temporal.TH.ActivityFn') provided by the 'discoverInstances' macro.
--
-- Thus, all compatible 'Workflow' and 'Activity' definitions in-scope are
-- auto-magically gathered; in a much more complex application, activities and
-- workflows could be defined in their own modules and then brought into scope
-- in one place for worker & client configuration.
workerConfig :: WorkerConfig ()
workerConfig = provideCallStack $ Worker.configure environment definitions settings
  where
    environment = ()
    definitions :: RequireCallStack => Worker.Definitions ()
    definitions = Temporal.TH.discoverDefinitions @() $$(discoverInstances) $$(discoverInstances)
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
main = bracket setup teardown $ \(withClient) -> do
  workflowId <- WorkflowId . UUID.toText <$> liftIO UUID.V4.nextRandom
  _result <-
    withClient $
      Client.execute
        SayHelloWorkflow
        workflowId
        (Client.startWorkflowOptions taskQueue)
        (SayHelloInput "World")
  pure ()
  where
    setup = do
      runtime <- initializeRuntime NoTelemetry
      coreClient <- runStdoutLoggingT $ connectClient runtime defaultClientConfig

      -- 'Client.execute' expects a context with @HasWorkflowClient@; for the
      -- sake of example, we can provide that here with a simple wrapper. 
      client <- workflowClient coreClient (mkWorkflowClientConfig namespace)
      let withClient action = runReaderT action client

      pure withClient

    teardown (_withClient) = pure ()
