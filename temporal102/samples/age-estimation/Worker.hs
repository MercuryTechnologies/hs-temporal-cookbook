module Main where

import Shared qualified

import Control.Monad (forever)
import Control.Monad.Logger (runStdoutLoggingT)
import Temporal.Core.Client (connectClient, defaultClientConfig)
import Temporal.Runtime (TelemetryOptions (NoTelemetry), initializeRuntime)
import Temporal.Worker qualified as Worker
import Temporal.Worker (startWorker)
import UnliftIO.Concurrent (threadDelay)
import UnliftIO.Exception (bracket)

main :: IO ()
main = bracket setup teardown do
  \_ -> forever $ threadDelay maxBound
  where
    setup = do
      runtime <- initializeRuntime NoTelemetry
      coreClient <- runStdoutLoggingT $ connectClient runtime defaultClientConfig
      manager <- Shared.newRelaxedEMSManager
      startWorker coreClient (Shared.mkWorkerConfig manager)
    teardown = Worker.shutdown
