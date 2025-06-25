module Main where

import Control.Monad (forever)
import Control.Monad.Logger (defaultOutput, runStdoutLoggingT)
import DiscoverInstances (discoverInstances)
import RequireCallStack (RequireCallStack, provideCallStack)
import System.IO (stdout)
import Temporal.Core.Client (connectClient, defaultClientConfig)
import Temporal.Runtime (TelemetryOptions (..), initializeRuntime)
import Temporal.TH (WorkflowFn, ActivityFn)
import Temporal.TH qualified
import Temporal.Worker (WorkerConfig, startWorker)
import Temporal.Worker qualified as Worker
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

-- | Main entrypoint - starts worker and keeps it running
main :: IO ()
main = bracket setup teardown $ \worker -> do
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

      pure worker

    teardown worker = Worker.shutdown worker
