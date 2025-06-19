module Exercise1 where

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

-- | Input data for the hello/goodbye translation workflow
data TranslationInput = TranslationInput
  { name :: Text           -- Name to greet
  , languageCode :: Text   -- Target language (fr, es, de, pt)
  } deriving (Generic, FromJSON, ToJSON, Show)

-- | Output data containing translated greetings
data TranslationOutput = TranslationOutput
  { helloMessage :: Text     -- Translated hello message
  , goodbyeMessage :: Text   -- Translated goodbye message
  } deriving (Generic, FromJSON, ToJSON, Show)

-- | Activity that translates by calling the translation service
translateActivity :: (Text, Text) -> Activity () Text
translateActivity (term, lang) = do
  req <- parseRequest $ "http://localhost:9001/translate/" ++ T.unpack term ++ "/" ++ T.unpack lang
  resp <- httpBS req
  pure . decodeUtf8 $ getResponseBody resp

-- Register the activity with Temporal's code generation
Temporal.TH.registerActivity 'translateActivity

-- | Workflow configuration options for activities
activityOptions :: Workflow.StartActivityOptions
activityOptions =
  Workflow.defaultStartActivityOptions (Workflow.StartToClose $ seconds 30)

-- | Main workflow that demonstrates durable execution
sayHelloGoodbyeWorkflow :: TranslationInput -> Workflow TranslationOutput
sayHelloGoodbyeWorkflow input = provideCallStack $ do
  -- First activity: translate "hello"
  hello <- Workflow.executeActivity TranslateActivity activityOptions ("hello", input.languageCode)
  let helloMsg = hello <> ", " <> input.name
  
  -- 10-second sleep: perfect time to test worker failure recovery
  Workflow.sleep (seconds 10)
  
  -- Second activity: translate "goodbye" 
  goodbye <- Workflow.executeActivity TranslateActivity activityOptions ("goodbye", input.languageCode)
  let goodbyeMsg = goodbye <> ", " <> input.name
  
  pure $ TranslationOutput helloMsg goodbyeMsg

-- Register the workflow with Temporal's code generation
Temporal.TH.registerWorkflow 'sayHelloGoodbyeWorkflow

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

-- | Main entrypoint - starts worker and keeps it running
runWorker :: IO ()
runWorker = bracket setup teardown $ \(withClient, worker) -> do
  putStrLn $ "Task Queue: " ++ show taskQueue
  putStrLn ""
  putStrLn "Ready to process workflows and activities."
  putStrLn "Kill this worker during workflow execution to test durable execution."
  putStrLn ""
  putStrLn "Press Ctrl+C to stop the worker"
  putStrLn ""
  
  forever $ threadDelay maxBound
  where
    setup = do
      runtime <- initializeRuntime NoTelemetry
      coreClient <- runStdoutLoggingT $ connectClient runtime defaultClientConfig

      worker <- startWorker coreClient workerConfig

      client <- workflowClient coreClient (mkWorkflowClientConfig namespace)
      let withClient action = runReaderT action client

      pure (withClient, worker)

    teardown (_withClient, worker) = Worker.shutdown worker

main :: IO ()
main = runWorker
