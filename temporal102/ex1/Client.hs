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

-- | Client that starts a workflow and exits immediately, leaving the workflow
-- running in the worker process. This demonstrates that workflows are durable
-- and continue executing after the client that started them terminates.

main :: IO ()
main = do
  args <- getArgs
  case args of
    [nameArg, langArg] -> do
      let input = TranslationInput (T.pack nameArg) (T.pack langArg)
      
      bracket setup teardown $ \withClient -> do
        workflowId <- WorkflowId . UUID.toText <$> liftIO UUID.V4.nextRandom
        putStrLn $ "Starting workflow with ID: " ++ show workflowId
        
        -- Start the workflow without waiting for completion.
        -- This demonstrates fire-and-forget workflow execution.
        handle <- withClient
          ( Client.start
              (SayHelloGoodbyeWorkflow)
              workflowId
              (Client.startWorkflowOptions taskQueue)
              input
          )
        
        putStrLn "Workflow started successfully!"
        putStrLn "The workflow will continue running even after this client exits."
        putStrLn ""
        putStrLn $ "Workflow ID: " ++ show workflowId
        pure ()
      
    _ -> do
      putStrLn "Usage: exercise1-client <name> <language-code>"
      putStrLn ""
      putStrLn "Supported language codes: fr, es, de, pt"
  where
    setup = do
      runtime <- initializeRuntime NoTelemetry
      coreClient <- runStdoutLoggingT $ connectClient runtime defaultClientConfig
      
      client <- workflowClient coreClient (mkWorkflowClientConfig namespace)
      let withClient action = runReaderT action client
      
      pure withClient
    
    teardown _withClient = pure ()
