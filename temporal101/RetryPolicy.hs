{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE DisambiguateRecordFields #-}
module Main where

import Control.Exception (throw)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Logger (defaultOutput, runStdoutLoggingT)
import Control.Monad.Trans.Reader (runReaderT)
import Data.Aeson (FromJSON, ToJSON)
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.Encoding (decodeUtf8)
import Data.UUID qualified as UUID
import Data.UUID.V4 qualified as UUID.V4
import DiscoverInstances (discoverInstances)
import GHC.Generics (Generic)
import Network.HTTP.Simple (httpBS, getResponseBody, getResponseStatusCode, parseRequest)
import RequireCallStack (RequireCallStack, provideCallStack)
import System.IO (stdout)
import Temporal.Activity (Activity)
import Temporal.Client (mkWorkflowClientConfig, workflowClient)
import Temporal.Client qualified as Client
import Temporal.Core.Client (connectClient, defaultClientConfig)
import Temporal.Duration (seconds)
import Temporal.Exception (ApplicationFailure (..))
import Temporal.Payload (JSON (JSON))
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
    -- | Throwing an ApplicationFailure fails the Activity. This can
    -- happen for many reasons
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

Temporal.TH.registerWorkflow 'webRequestWorkflow

taskQueue :: Workflow.TaskQueue
taskQueue = "activity-task-queue"

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
