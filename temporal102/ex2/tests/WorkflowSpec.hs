module WorkflowSpec (spec) where

import TestUtils

import Workflow (
    SayHelloGoodbyeWorkflow(..),
    TranslationInput(..),
    TranslationOutput(..),
  )

import Control.Monad.Logger (defaultOutput)
import Control.Monad.Trans.Reader (ReaderT, runReaderT)
import DiscoverInstances (discoverInstances)
import RequireCallStack (RequireCallStack, provideCallStack)
import System.IO (stdout)
import Test.Hspec
import Test.Hspec.Expectations
import Temporal.Client (WorkflowClient)
import qualified Temporal.Client as Client
import Temporal.TH (WorkflowFn, ActivityFn)
import qualified Temporal.TH
import Temporal.Worker (WorkerConfig)
import qualified Temporal.Worker as Worker
import qualified Temporal.Workflow as Workflow

-- | Prepare the time-skipping server & worker process on a free port, then
-- pass a 'WorkflowClient' that can communicate with them to the test suite.
spec :: Spec
spec = aroundAll withTimeSkippingServer
  . aroundAllWith (withWorker workerConfig)
  . aroundAllWith (withTimeSkippingClient namespace)
  $ workflowSpec

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
    pending

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

-- | Default 'Workflow.Namespace' for this set of tests.
--
-- Shared between 'Worker.Worker' and 'WorkerClient' so that tests can execute
-- on the 'Worker.Worker' spawned by the test harness.
namespace :: Workflow.Namespace
namespace = "default"

-- | Default 'Workflow.TaskQueue' for this set of tests.
--
-- Shared between 'Worker.Worker' and 'WorkerClient' so that tests can execute
-- on the 'Worker.Worker' spawned by the test harness.
taskQueue :: Workflow.TaskQueue
taskQueue = "test"
