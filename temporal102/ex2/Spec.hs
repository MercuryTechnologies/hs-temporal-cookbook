{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedRecordDot #-}

module Main where

import Control.Exception (bracket, throwIO)
import Control.Monad.Logger (defaultOutput, runStdoutLoggingT)
import Control.Monad.Trans.Reader (ReaderT, runReaderT)
import Data.Functor ((<&>))
import qualified Data.Text as Text
import DiscoverInstances (discoverInstances)
import System.Directory (findExecutable)
import System.IO (stdout)
import System.IO.Unsafe (unsafePerformIO)
import RequireCallStack (RequireCallStack, provideCallStack)
import Temporal.Client (WorkflowClient, WorkflowClientConfig, mkWorkflowClientConfig, workflowClient)
import qualified Temporal.Client as Client
import Temporal.Core.Client (ClientConfig (..), connectClient, defaultClientConfig)
import qualified Temporal.Core.Client as Core
import Temporal.Exception (ApplicationFailure (..))
import Temporal.Testing.MockActivityEnvironment (mkMockActivityEnvironment, runMockActivity)
import Temporal.EphemeralServer (
    EphemeralExe(..),
    PortNumber,
    TemporalTestServerConfig(..),
    getFreePort,
    withTestServer
  )
import Temporal.Runtime (TelemetryOptions(NoTelemetry), Runtime, initializeRuntime)
import Temporal.TH (WorkflowFn, ActivityFn)
import qualified Temporal.TH
import Temporal.Worker (WorkerConfig)
import qualified Temporal.Worker as Worker
import qualified Temporal.Workflow as Workflow
import Test.Hspec
import Test.Hspec.Expectations
import Workflow (
    SayHelloGoodbyeWorkflow(..),
    TranslateTermInput(..),
    TranslationInput(..),
    TranslationOutput(..),
    translateActivity
  )

main :: IO ()
main = hspec do
  activitySpec
  aroundAll withTimeSkippingServer
    . aroundAllWith (withWorker workerConfig)
    . aroundAllWith withTimeSkippingClient
    $ workflowSpec

-- | Test the translation activity in a mocked environment.
activitySpec :: Spec
activitySpec = describe "translation activity" do
  it "successfully translates 'hello' to German" do
    mockEnv <- mkMockActivityEnvironment ()
    let input = TranslateTermInput "Hello" "de"
    res <- runMockActivity mockEnv $ translateActivity input
    res `shouldBe` "hallo"
  it "successfully translates 'goodbye' to Latvian" do
    mockEnv <- mkMockActivityEnvironment ()
    let input = TranslateTermInput "Goodbye" "lv"
    res <- runMockActivity mockEnv $ translateActivity input
    res `shouldBe` "ardievu"
  it "fails to translate with bad language code" do
    pending

-- | Test the translation workflow using the time-skipping test server.
workflowSpec :: SpecWith WorkflowClient
workflowSpec = describe "translation workflow" do
  it "successfully completes French translation" \client -> do
    output <- flip runReaderT client do
      Client.execute
        SayHelloGoodbyeWorkflow
        "test"
        (Client.startWorkflowOptions taskQueue)
        (TranslationInput "Pierre" "fr")
    output.helloMessage `shouldBe` "bonjour, Pierre"
    output.goodbyeMessage `shouldBe` "au revoir, Pierre"

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
    coreClient <- runStdoutLoggingT $ connectClient globalRuntime coreConfig
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
withTimeSkippingClient :: ActionWith WorkflowClient -> ActionWith Core.Client
withTimeSkippingClient action coreClient = do
  client <- workflowClient coreClient $ (mkWorkflowClientConfig namespace) {
      Client.enableTimeSkipping = True
    }
  action client

-- | A global Tokio 'Runtime' scoped to the test process.
globalRuntime :: Runtime
globalRuntime = unsafePerformIO $ initializeRuntime NoTelemetry
{-# NOINLINE globalRuntime #-}

-- | Construct a 'WorkerConfig' that supports 'SayHelloGoodbyeWorkflow'.
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

namespace :: Workflow.Namespace
namespace = "default"

taskQueue :: Workflow.TaskQueue
taskQueue = "test"
