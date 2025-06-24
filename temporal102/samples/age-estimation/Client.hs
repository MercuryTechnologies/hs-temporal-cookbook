module Main where

import Shared qualified
import Workflow (EstimateAgeWorkflow (..))

import Control.Monad.IO.Class (liftIO)
import Control.Monad.Logger (runStdoutLoggingT)
import Control.Monad.Trans.Reader (runReaderT)
import Data.Functor (void)
import Data.Text qualified as Text
import Data.UUID qualified as UUID
import Data.UUID.V4 qualified as UUID.V4
import System.Environment (getArgs)
import Temporal.Client (mkWorkflowClientConfig, workflowClient)
import Temporal.Client qualified as Client
import Temporal.Core.Client (connectClient, defaultClientConfig)
import Temporal.Runtime (TelemetryOptions (NoTelemetry), initializeRuntime)
import Temporal.Workflow (WorkflowHandle (..), WorkflowId (..))

main :: IO ()
main = do
  args <- getArgs
  case args of
    [name] -> do
      workflowId <- WorkflowId . UUID.toText <$> liftIO UUID.V4.nextRandom
      putStrLn $ "Starting workflow with ID: " ++ show workflowId

      client <- mkClient
      -- Start the workflow without waiting for completion.
      -- 
      -- This demonstrates fire-and-forget workflow execution.
      handle <- flip runReaderT client $ Client.start
        EstimateAgeWorkflow
        workflowId
        (Client.startWorkflowOptions Shared.taskQueue)
        (Text.pack name)

      let resolvedWorkflowId = rawWorkflowId . Client.workflowHandleWorkflowId $ handle
      putStrLn $ "Started workflow " <> Text.unpack resolvedWorkflowId

      result <- Client.waitWorkflowResult handle
      putStrLn $ Text.unpack result
      
    _ -> do
      putStrLn "Must specify a name as the command-line argument"
  where
    mkClient = do
      runtime <- initializeRuntime NoTelemetry
      coreClient <- runStdoutLoggingT $ connectClient runtime defaultClientConfig
      workflowClient coreClient (mkWorkflowClientConfig Shared.namespace)
  
