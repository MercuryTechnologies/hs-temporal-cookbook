{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE DisambiguateRecordFields #-}
module Main where

import Control.Exception (throw)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Logger (defaultOutput, runStdoutLoggingT)
import Control.Monad.Trans.Reader (runReaderT)
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.Encoding (decodeUtf8)
import Data.UUID qualified as UUID
import Data.UUID.V4 qualified as UUID.V4
import DiscoverInstances (discoverInstances)
import Network.HTTP.Simple (httpBS, getResponseBody, getResponseStatusCode, parseRequest)
import RequireCallStack (RequireCallStack, provideCallStack)
import System.IO (stdout)
import Temporal.Activity (Activity)
import Temporal.Client (mkWorkflowClientConfig, workflowClient)
import Temporal.Client qualified as Client
import Temporal.Core.Client (connectClient, defaultClientConfig)
import Temporal.Duration (seconds)
import Temporal.Exception (ApplicationFailure (..))
import Temporal.Runtime (TelemetryOptions (..), initializeRuntime)
import Temporal.TH (WorkflowFn, ActivityFn)
import Temporal.TH qualified
import Temporal.Worker (Worker, WorkerConfig, startWorker)
import Temporal.Worker qualified as Worker
import Temporal.Workflow (Workflow, WorkflowId (..))
import Temporal.Workflow qualified as StartActivityOptions (StartActivityOptions (..))
import Temporal.Workflow qualified as Workflow
import UnliftIO.Exception (bracket)

-- | An Activity that calls a local webserver. Activity has a MonadIO
-- instance so we don't need to provide anything in the environment,
-- which remains @()@. This time, our Activity adds some error handling
-- in case the response it gets is not the 200 it expects.
trySometimesBusy :: Activity () Text
trySometimesBusy = do
  resp <- httpBS "http://localhost:9001/sometimesBusy"
  let status = getResponseStatusCode resp
  case status of
    200 -> pure . decodeUtf8 $ getResponseBody resp
    _ -> errorOut status
  where
    -- | Throwing an ApplicationFailure fails the Activity. Temporal
    -- workflows are expected to be robust against activity failures,
    -- though, so by default this won't fail the Workflow's execution.
    errorOut status = throw 
      ApplicationFailure
        { type' = "DidntGet200Error"
        , message = "Expected a 200 but got a " <> tshow status
        , nonRetryable = False
        , details = []
        , stack = ""
        , nextRetryDelay = Nothing
        }
    tshow = Text.pack . show

Temporal.TH.registerActivity 'trySometimesBusy

-- | We'll define a retry policy explicitly (default values are
-- indicated in comments). The default for 'maximumAttempts' is 0, which
-- actually means "unlimited" -- note that this isn't a cap on number of
-- _retries_, it's a cap on _total executions_. Here, we're setting a
-- small number to illustrate workflow failure downstream of activity
-- failure.
retryPolicy :: Workflow.RetryPolicy
retryPolicy =
  Workflow.defaultRetryPolicy
    { -- initialInterval = seconds 1
      -- backoffCoefficient = 2.0
      -- maximumInterval = seconds 100
      Workflow.maximumAttempts = 2
      -- nonRetryableErrorTypes = []
    }

activityOptions :: Workflow.StartActivityOptions
activityOptions =
  (Workflow.defaultStartActivityOptions (Workflow.StartToClose $ seconds 10))
    { StartActivityOptions.retryPolicy = Just retryPolicy }
  
webRequestWorkflow :: Workflow Text
webRequestWorkflow = provideCallStack do
  Workflow.executeActivity TrySometimesBusy activityOptions

-- | The rest of this file is typical of what you've seen in previous
-- exercises; it configures a worker, starts it, and executes a workflow
-- against it.
Temporal.TH.registerWorkflow 'webRequestWorkflow

taskQueue :: Workflow.TaskQueue
taskQueue = "activity-task-queue"

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
        WebRequestWorkflow
        workflowId
        (Client.startWorkflowOptions taskQueue)
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
