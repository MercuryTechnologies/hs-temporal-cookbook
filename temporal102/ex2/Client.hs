module Main where

import Control.Monad.IO.Class (liftIO)
import Control.Monad.Logger (runStdoutLoggingT)
import Control.Monad.Trans.Reader (runReaderT)
import Data.UUID qualified as UUID
import Data.UUID.V4 qualified as UUID.V4
import System.Environment
import qualified Data.Text as T
import Temporal.Client (mkWorkflowClientConfig, workflowClient)
import Temporal.Client qualified as Client
import Temporal.Core.Client (connectClient, defaultClientConfig)
import Temporal.Runtime (TelemetryOptions (..), initializeRuntime)
import Temporal.Workflow (WorkflowId (..))
import Temporal.Workflow qualified as Workflow
import UnliftIO.Exception (bracket)
import Workflow

taskQueue :: Workflow.TaskQueue
taskQueue = "translation-tasks"

namespace :: Workflow.Namespace
namespace = "default"

main :: IO ()
main = do
  args <- getArgs
  case args of
    [nameArg, langArg] -> do
      let input = TranslationInput (T.pack nameArg) (T.pack langArg)

      client <- mkClient
      
      workflowId <- WorkflowId . UUID.toText <$> liftIO UUID.V4.nextRandom
      putStrLn $ "Starting workflow with ID: " ++ show workflowId
      
      handle <- flip runReaderT client $
        Client.start
          SayHelloGoodbyeWorkflow
          workflowId
          (Client.startWorkflowOptions taskQueue)
          input
        
      pure ()
      
    _ -> do
      putStrLn "Must supply a name and language code as arguments"
  where
    mkClient = do
      runtime <- initializeRuntime NoTelemetry
      coreClient <- runStdoutLoggingT $ connectClient runtime defaultClientConfig
      workflowClient coreClient (mkWorkflowClientConfig namespace)
