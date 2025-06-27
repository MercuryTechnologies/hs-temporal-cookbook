module MockWorkflowSpec (spec) where

import TestUtils

import Workflow (
    sayHelloGoodbyeWorkflow,
    SayHelloGoodbyeWorkflow(..),
    TranslateActivity(..),
    TranslateTermInput(..),
    TranslationInput(..),
    TranslationOutput(..),
  )

import Control.Monad.Logger (defaultOutput)
import Control.Monad.Trans.Reader (ReaderT, runReaderT)
import Data.Text (Text)
import DiscoverInstances (discoverInstances)
import RequireCallStack (RequireCallStack, provideCallStack)
import System.IO (stderr)
import Test.Hspec
import Test.Hspec.Expectations
import Temporal.Activity (
  Activity,
  ProvidedActivity,
  provideActivity
  )
import Temporal.Client (WorkflowClient)
import qualified Temporal.Client as Client
import Temporal.Payload (JSON (JSON))
import Temporal.TH (WorkflowFn, ActivityFn)
import qualified Temporal.TH
import Temporal.Worker (WorkerConfig)
import qualified Temporal.Worker as Worker
import Temporal.Workflow (
  ProvidedWorkflow,
  Workflow,
  activityRef,
  knownActivityName,
  knownWorkflowName,
  provideWorkflow,
  workflowRef,
  )
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
    output.helloMessage `shouldBe` "bonjour, Pierre"
    output.goodbyeMessage `shouldBe` "au revoir, Pierre"

-- | Construct a 'WorkerConfig' that supports a stubbed
-- `SayHelloGoodbyeWorkflow`. We can't do the usual Template Haskell
-- workflow and activity discovery because that'll pick up the real
-- activity, so we'll just claim that they exist instead. This is the
-- same `knownActivity` mechanism we used to call a foreign-language
-- activity in Temporal101 ex4.
workerConfig :: WorkerConfig ()
workerConfig = provideCallStack $ Worker.configure () definitions settings
  where
    definitions = (activity, workflow)
    settings = do
      Worker.setNamespace namespace
      Worker.setTaskQueue taskQueue
      Worker.setLogger (defaultOutput stderr)

namespace :: Workflow.Namespace
namespace = "default"

taskQueue :: Workflow.TaskQueue
taskQueue = "test"

-- | Manually construct a `ProvideWorkflow` for SayHelloGoodbyeWorkflow, 
-- since we're avoiding TH. Compare this to hello/ActivityWithBoilerplate
-- where we do something similar.
workflow :: ProvidedWorkflow (TranslationInput -> Workflow TranslationOutput)
workflow = provideWorkflow JSON name sayHelloGoodbyeWorkflow
  where
    name = knownWorkflowName (workflowRef SayHelloGoodbyeWorkflow)

-- | Construct a stubbed version of TranslateActivity
activity :: ProvidedActivity () (TranslateTermInput -> Activity () Text)
activity = provideActivity JSON name stub
  where
    name = knownActivityName (activityRef TranslateActivity)

    -- | Our stubbed-out activity. This just needs to conform to the
    -- signature of the real Activity implementation.
    stub :: TranslateTermInput -> Activity () Text
    stub TranslateTermInput{..} =
      case term of
        "hello" -> pure "bonjour"
        "goodbye" -> pure "au revoir"
        _ -> undefined
