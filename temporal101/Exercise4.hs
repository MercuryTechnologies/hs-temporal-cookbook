{-# LANGUAGE DataKinds #-}
module Main where

import Control.Monad.IO.Class (liftIO)
import Control.Monad.Logger (defaultOutput, runStdoutLoggingT)
import Control.Monad.Trans.Reader (runReaderT)
import Data.Aeson (FromJSON, ToJSON)
import Data.Text (Text)
import Data.Text qualified as Text
import Data.UUID qualified as UUID
import Data.UUID.V4 qualified as UUID.V4
import DiscoverInstances (discoverInstances)
import GHC.Generics (Generic)
import RequireCallStack (RequireCallStack, provideCallStack)
import System.IO (stdout)
import Temporal.Activity (KnownActivity (..))
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

data CreatePdfInput = CreatePdfInput
  { name :: Text
  }
  deriving stock (Generic, Show)
  deriving anyclass (FromJSON, ToJSON)

-- | The Java worker defines the "CreatePdf' activity, but we need to
-- know its name and interface (type signature, more or less) to invoke
-- it. We use 'KnownActivity' for that.
createPdf :: KnownActivity '[Text] Text
createPdf = KnownActivity JSON "CreatePdf"

-- | We still need activity options to invoke the Activity
activityOptions :: Workflow.StartActivityOptions
activityOptions =
  Workflow.defaultStartActivityOptions (Workflow.StartToClose $ seconds 1)

-- | This is no different from executing an activity we've written.
graduationWorkflow :: CreatePdfInput -> Workflow Text
graduationWorkflow input = provideCallStack do
  Workflow.executeActivity createPdf activityOptions input.name

Temporal.TH.registerWorkflow 'graduationWorkflow

taskQueue :: Workflow.TaskQueue
taskQueue = "generate-certificate-taskqueue"

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
        GraduationWorkflow
        workflowId
        (Client.startWorkflowOptions taskQueue)
        (CreatePdfInput "Matt Olson")
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
