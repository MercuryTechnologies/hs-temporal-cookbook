module Main where

import Exercise1 (TranslationInput(..), TranslationOutput(..), taskQueue, namespace)
import UsingRecordTypes (SayHelloGoodbyeWorkflowWithRecords(..))
import Control.Monad.Logger (runStdoutLoggingT)
import Control.Monad.Trans.Reader (runReaderT)
import Data.UUID qualified as UUID
import Data.UUID.V4 qualified as UUID.V4
import System.Environment (getArgs)
import Data.Text qualified as T
import Data.Text.Encoding (decodeUtf8, encodeUtf8)
import Temporal.Client (mkWorkflowClientConfig, workflowClient)
import Temporal.Client qualified as Client
import Temporal.Core.Client (connectClient, defaultClientConfig)
import Temporal.Runtime (TelemetryOptions (..), initializeRuntime)
import Temporal.Workflow (WorkflowId (..))
import UnliftIO.Exception (bracket)

main :: IO ()
main = do
  args <- getArgs
  case args of
    [nameArg, langArg] -> do
      let input = TranslationInput (T.pack nameArg) (T.pack langArg)
      
      bracket setup teardown $ \withClient -> do
        workflowId <- WorkflowId . UUID.toText <$> UUID.V4.nextRandom
        putStrLn $ "Starting workflow with ID: " ++ show workflowId
        putStrLn $ "Input record: " ++ show input
        putStrLn ""
        putStrLn "This workflow demonstrates structured record types"
        putStrLn ""
        
        result <- withClient
          ( Client.execute
              SayHelloGoodbyeWorkflowWithRecords
              workflowId
              (Client.startWorkflowOptions taskQueue)
              input
          )
        
        putStrLn "Workflow completed!"
        putStrLn $ "Output record: " ++ show result
        putStrLn ""
        putStrLn $ "Hello: " ++ T.unpack result.helloMessage
        putStrLn $ "Goodbye: " ++ T.unpack result.goodbyeMessage

  where
    setup = do
      runtime <- initializeRuntime NoTelemetry
      coreClient <- runStdoutLoggingT $ connectClient runtime defaultClientConfig
      client <- workflowClient coreClient (mkWorkflowClientConfig namespace)
      let withClient action = runReaderT action client
      pure withClient
    teardown _ = pure ()