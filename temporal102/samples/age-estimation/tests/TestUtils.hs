module TestUtils where

import Shared qualified

import qualified Data.Text as Text
import Control.Monad.Logger (runStdoutLoggingT)
import Network.HTTP.Client qualified as HTTP.Client
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
    TemporalDevServerConfig(..),
    defaultTemporalDevServerConfig,
    getFreePort,
  )
import Temporal.EphemeralServer qualified as EphemeralServer
import Temporal.Runtime (TelemetryOptions(NoTelemetry), Runtime, initializeRuntime)
import Temporal.Worker (WorkerConfig)
import qualified Temporal.Worker as Worker
import qualified Temporal.Workflow as Workflow
import UnliftIO.Exception (bracket, throwIO)

-- | Construct an ephemeral test server running on a free port and make it,
-- along with connected 'Core.Client', available within the scope of some
-- function under test.
--
-- Typically precedes a call to 'withTimeSkippingClient'.
withDevServer :: (Core.Client -> IO a) -> IO a
withDevServer action = do
  port <- getFreePort
  config <- findExecutable "temporal" >>= \case
    Nothing -> throwIO $ userError "unable to find 'temporal'"
    Just exe -> pure defaultTemporalDevServerConfig {
        exe = ExistingPath exe,
        port = Just $ fromIntegral port
      }
  EphemeralServer.withDevServer globalRuntime config \_server -> do
    let coreConfig = defaultClientConfig { targetUrl = "http://localhost:" <> (Text.pack . show $ port) }
    coreClient <- runStdoutLoggingT $ connectClient globalRuntime coreConfig
    action coreClient

-- | Construct a 'Worker' connected to a server associated with the given
-- 'Core.Client' and make it available to the function under test.
--
-- Typically follows a call to set up an ephemeral server, such as
-- 'withDevServer'.
withWorker :: WorkerConfig env -> ActionWith Core.Client -> ActionWith Core.Client
withWorker config action coreClient = do
  bracket (Worker.startWorker coreClient config) Worker.shutdown \_ -> do
    action coreClient

-- | Construct a 'WorkflowClient' connected to a server associated with the
-- given 'Core.Client' and make it available to the function under test.
--
-- Typically follows a call to 'withDevServer'.
withTestClient :: Workflow.Namespace -> ActionWith WorkflowClient -> ActionWith Core.Client
withTestClient namespace action coreClient = do
  client <- workflowClient coreClient $ (mkWorkflowClientConfig namespace)
  action client

-- | A global Tokio 'Runtime' scoped to the test process.
globalRuntime :: Runtime
globalRuntime = unsafePerformIO $ initializeRuntime NoTelemetry
{-# NOINLINE globalRuntime #-}

-- | A global HTTP connection manager with TLS settings that permit responses
-- without an Extended Main Secret.
globalRelaxedEMSManager :: HTTP.Client.Manager
globalRelaxedEMSManager = unsafePerformIO Shared.newRelaxedEMSManager
{-# NOINLINE globalRelaxedEMSManager #-}
