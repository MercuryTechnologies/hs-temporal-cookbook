module Main where

import Control.Monad (forever)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Logger (defaultOutput, runStdoutLoggingT)
import Control.Monad.Trans.Reader (runReaderT)
import Data.Aeson (FromJSON, ToJSON)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding (decodeUtf8)
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
import Temporal.Runtime (TelemetryOptions (..), initializeRuntime)
import Temporal.TH (WorkflowFn, ActivityFn)
import Temporal.TH qualified
import Temporal.Worker (Worker, WorkerConfig, startWorker)
import Temporal.Worker qualified as Worker
import Temporal.Workflow (Workflow, WorkflowId (..))
import Temporal.Workflow qualified as Workflow
import UnliftIO.Concurrent (threadDelay)
import UnliftIO.Exception (bracket)
import Workflow

-- | Task queue name
taskQueue :: Workflow.TaskQueue
taskQueue = "translation-tasks"

-- | Temporal namespace
namespace :: Workflow.Namespace
namespace = "default"

-- | Worker configuration
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
main = bracket setup teardown $ \worker -> do
  forever $ threadDelay maxBound
  where
    setup = do
      runtime <- initializeRuntime NoTelemetry
      coreClient <- runStdoutLoggingT $ connectClient runtime defaultClientConfig

      worker <- startWorker coreClient workerConfig

      pure worker

    teardown worker = Worker.shutdown worker
