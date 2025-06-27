{-# LANGUAGE LambdaCase #-}

module TestUtils where

import Control.Exception (bracket, throwIO)
import qualified Data.Text as Text
import Control.Monad.Logger (runStderrLoggingT)
import System.Directory (findExecutable)
import System.IO.Unsafe (unsafePerformIO)
import Test.Hspec
import qualified Temporal.Client as Client
import Temporal.Client (WorkflowClient, WorkflowClientConfig, mkWorkflowClientConfig, workflowClient)
import Temporal.Core.Client (ClientConfig (..), connectClient, defaultClientConfig)
import qualified Temporal.Core.Client as Core
import Temporal.EphemeralServer (
    EphemeralExe(..),
    PortNumber,
    TemporalTestServerConfig(..),
    getFreePort,
    withTestServer
  )
import Temporal.Runtime (TelemetryOptions(NoTelemetry), Runtime, initializeRuntime)
import Temporal.Worker (WorkerConfig)
import qualified Temporal.Worker as Worker
import qualified Temporal.Workflow as Workflow

-- | Construct an ephemeral test server running on a free port and make it,
-- along with connected 'Core.Client', available within the scope of some
-- function under test.
--
-- Typically precedes a call to 'withTimeSkippingClient'.
withTimeSkippingServer :: (Core.Client -> IO a) -> IO a
withTimeSkippingServer action = do
  port <- getFreePort
  config <- findExecutable "temporal-test-server" >>= \case
    Nothing -> throwIO $ userError "unable to find 'temporal-test-server'"
    Just exe -> pure TemporalTestServerConfig {
        exe = ExistingPath exe,
        port = Just $ fromIntegral port,
        extraArgs = []
      }
  withTestServer globalRuntime config \_server -> do
    let coreConfig = defaultClientConfig { targetUrl = "http://localhost:" <> (Text.pack . show $ port) }
    coreClient <- runStderrLoggingT $ connectClient globalRuntime coreConfig
    action coreClient

-- | Construct a 'Worker' connected to a server associated with the given
-- 'Core.Client' and make it available to the function under test.
--
-- Typically follows a call to set up an ephemeral server, such as
-- 'withTimeSkippingServer'.
withWorker :: WorkerConfig env -> ActionWith Core.Client -> ActionWith Core.Client
withWorker config action coreClient = do
  bracket (Worker.startWorker coreClient config) Worker.shutdown \_ -> do
    action coreClient

-- | Construct a time-skipping 'WorkflowClient' connected to a server
-- associated with the given 'Core.Client' and make it available to the
-- function under test.
--
-- Typically follows a call to 'withTimeSkippingServer'.
withTimeSkippingClient :: Workflow.Namespace -> ActionWith WorkflowClient -> ActionWith Core.Client
withTimeSkippingClient namespace action coreClient = do
  client <- workflowClient coreClient $ (mkWorkflowClientConfig namespace) {
      Client.enableTimeSkipping = True
    }
  action client

-- | A global Tokio 'Runtime' scoped to the test process.
globalRuntime :: Runtime
globalRuntime = unsafePerformIO $ initializeRuntime NoTelemetry
{-# NOINLINE globalRuntime #-}

