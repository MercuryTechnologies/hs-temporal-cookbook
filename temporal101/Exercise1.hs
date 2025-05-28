module Main where

import Control.Monad (forever)
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
import Temporal.TH (WorkflowFn, ActivityFn)
import Temporal.TH qualified
import Temporal.Worker (Worker, WorkerConfig, startWorker)
import Temporal.Worker qualified as Worker
import Temporal.Workflow (Workflow, WorkflowId (..))
import Temporal.Workflow qualified as Workflow
import UnliftIO.Concurrent (threadDelay)
import UnliftIO.Exception (bracket)

-- | The official "Hello, Workflow" example takes a bare string as input.
-- Here we reinforce the pattern of creating a record type for inputs so
-- that it can evolve freely.
data SayHelloInput = SayHelloInput
  { name :: Text
  }
  deriving stock (Generic, Show)
  deriving anyclass (FromJSON, ToJSON)

-- sayHelloActivity :: SayHelloInput -> Activity () Text
-- sayHelloActivity input = do
--   pure $ "Hello, " <> input.name

-- Temporal.TH.registerActivity 'sayHelloActivity
-- Temporal.TH.bringRegisteredTemporalFunctionsIntoScope

-- activityOptions :: Workflow.StartActivityOptions
-- activityOptions = Workflow.defaultStartActivityOptions (Workflow.StartToClose $ seconds 1)

-- | We don't execute an activity in this workflow, we execute code
-- directly (why?).
sayHelloWorkflow :: SayHelloInput -> Workflow Text
sayHelloWorkflow input = provideCallStack do
  pure $ "Hello, " <> input.name
  -- Workflow.executeActivity SayHelloActivity activityOptions input

Temporal.TH.registerWorkflow 'sayHelloWorkflow
Temporal.TH.bringRegisteredTemporalFunctionsIntoScope

taskQueue :: Workflow.TaskQueue
taskQueue = "hello-world"

namespace :: Workflow.Namespace
namespace = "default"

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

main :: IO ()
main = bracket setup teardown $ \_worker -> do
  -- the worker's running in its own thread, we just have to keep this
  -- one alive (one one-second threadDelay at a time) for as long as we
  -- want to execute workflows on it
  forever $ threadDelay 1_000_000
  pure ()
  where
    setup = do
      runtime <- initializeRuntime NoTelemetry
      -- coreClient is needed to construct a worker, even though we're
      -- not constructing a client
      coreClient <- connectClient runtime defaultClientConfig

      -- startWorker starts a Temporal worker and returns a handle to
      -- it. the Temporal control plane can dispatch to the worker, or
      -- you can run workflows on it directly
      worker <- startWorker coreClient workerConfig

      pure worker

    teardown worker = Worker.shutdown worker
