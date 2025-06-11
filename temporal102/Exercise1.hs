module Exercise1 where

import Control.Monad (forever)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Logger (defaultOutput, runStdoutLoggingT)
import DiscoverInstances (discoverInstances)
import RequireCallStack (RequireCallStack, provideCallStack)
import System.IO (stdout)
import Temporal.Core.Client (connectClient, defaultClientConfig)
import Temporal.Runtime (TelemetryOptions (..), initializeRuntime)
import Temporal.TH (WorkflowFn, ActivityFn)
import Temporal.TH qualified
import Temporal.Worker (Worker, WorkerConfig, startWorker)
import Temporal.Worker qualified as Worker
import Temporal.Workflow qualified as Workflow
import Temporal.Activity (Activity)
import Temporal.Duration (seconds)
import UnliftIO.Concurrent (threadDelay)
import UnliftIO.Exception (bracket)
import Data.Aeson
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Map as Map
import GHC.Generics (Generic)

-- | Exercise 1: Durable Execution
-- 
-- This exercise demonstrates Temporal's core feature: durable execution.
-- Workflows will continue to run, even when workers are killed and restarted.
-- 
-- Key concepts:
-- - Workflows survive worker failures
-- - State is automatically persisted and recovered
-- - No duplicate work is performed during failover

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

-- | Translation dictionary mapping language codes to term translations
-- Supports French (fr), Spanish (es), and German (de)
translations :: Map.Map Text (Map.Map Text Text)
translations = Map.fromList
  [ ("fr", Map.fromList [("hello", "bonjour"), ("goodbye", "au revoir")])
  , ("es", Map.fromList [("hello", "hola"), ("goodbye", "adiós")])
  , ("de", Map.fromList [("hello", "hallo"), ("goodbye", "auf wiedersehen")])
  , ("pt", Map.fromList [("hello", "olá"), ("goodbye", "tchau")])
  ]

-- Activities are the basic unit of work in Temporal
-- side effects and can be retried automatically on failure
translateActivity :: (Text, Text) -> Activity () Text
translateActivity (term, lang) = do
  case Map.lookup lang translations >>= Map.lookup (T.toLower term) of
    Just translation -> return translation
    Nothing -> return term

-- Register the activity with Temporal's code generation
Temporal.TH.registerActivity 'translateActivity

-- | Main workflow that demonstrates durable execution
-- 
-- This workflow:
-- 1. Translates "hello" to the target language
-- 2. Sleeps for 10 seconds (prime time to kill workers!)
-- 3. Translates "goodbye" to the target language
-- 4. Returns both translated messages
--
-- The 10-second sleep provides a window to test
-- durable execution by killing workers during the sleep period.
sayHelloGoodbyeWorkflow :: TranslationInput -> Workflow.Workflow TranslationOutput
sayHelloGoodbyeWorkflow input = provideCallStack $ do
  -- First activity: translate "hello"
  hello <- Workflow.executeActivity TranslateActivity activityOptions ("hello", input.languageCode)
  let helloMsg = hello <> ", " <> input.name
  
  -- 10-second sleep: perfect time to test worker failure recovery
  Workflow.sleep (seconds 10)
  
  -- Second activity: translate "goodbye" 
  goodbye <- Workflow.executeActivity TranslateActivity activityOptions ("goodbye", input.languageCode)
  let goodbyeMsg = goodbye <> ", " <> input.name
  
  return $ TranslationOutput helloMsg goodbyeMsg
  where
    activityOptions = Workflow.defaultStartActivityOptions (Workflow.StartToClose $ seconds 30)

-- Register the workflow with Temporal's code generation
Temporal.TH.registerWorkflow 'sayHelloGoodbyeWorkflow

-- Bring generated constructors into scope
Temporal.TH.bringRegisteredTemporalFunctionsIntoScope

-- | Task queue name
taskQueue :: Workflow.TaskQueue
taskQueue = "translation-tasks"

-- | Temporal namespace
namespace :: Workflow.Namespace
namespace = "default"

-- | Worker configuration specifying task queue, namespace, and available workflows/activities
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

-- | Start a worker that processes workflows and activities
-- 
-- The worker runs indefinitely, polling for tasks from the Temporal server.
-- Multiple workers can run simultaneously for load balancing and fault tolerance.
runWorker :: IO ()
runWorker = do
  runtime <- initializeRuntime NoTelemetry
  coreClient <- runStdoutLoggingT $ connectClient runtime defaultClientConfig
  worker <- startWorker coreClient workerConfig
  
  putStrLn "Worker started! Processing tasks from queue: translation-tasks"
  putStrLn ""
  putStrLn "Ready to process workflows. Kill this worker during execution"
  putStrLn "to test Temporal's durable execution capabilities."
  putStrLn ""
  putStrLn "Press Ctrl+C to stop the worker"
  
  forever $ threadDelay maxBound