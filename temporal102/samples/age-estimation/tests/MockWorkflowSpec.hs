module MockWorkflowSpec (spec) where

import Workflow (
    EstimateAge (..),
    EstimateAgeWorkflow (..),
    estimateAgeWorkflow
  )
import Shared
import TestUtils

import Control.Monad.Logger (defaultOutput)
import Control.Monad.Reader (runReaderT)
import Data.Text (Text)
import DiscoverInstances (discoverInstances)
import RequireCallStack (RequireCallStack, provideCallStack)
import System.IO (stdout)
import Temporal.Activity (
    Activity,
    ProvidedActivity,
    provideActivity
  )
import Temporal.Client qualified as Client
import Temporal.Client (WorkflowClient)
import Temporal.Payload (JSON (JSON))
import Temporal.TH (WorkflowFn, ActivityFn)
import Temporal.TH qualified
import Temporal.Worker (WorkerConfig)
import Temporal.Worker qualified as Worker
import Temporal.Workflow (
    ProvidedWorkflow,
    Workflow,
    activityRef,
    knownActivityName,
    knownWorkflowName,
    provideWorkflow,
    workflowRef,
  )
import Test.Hspec
import Test.Hspec.Expectations

spec :: Spec
spec =
  aroundAll withDevServer
    . aroundAllWith (withWorker workerConfig)
    . aroundAllWith (withTestClient Shared.namespace)
    $ mockWorkflowSpec

mockWorkflowSpec :: SpecWith WorkflowClient
mockWorkflowSpec = describe "mocked EstimateAge workflow" do
  it "runs estimateAgeWorkflow with a mocked activity call" \client -> do
    message <- flip runReaderT client do
      Client.execute
        EstimateAgeWorkflow
        "test"
        (Client.startWorkflowOptions Shared.taskQueue)
        "Betty"
    message `shouldBe` "Betty has an estimated age of 78"

-- | Configure a worker that dispatches incoming 'EstimateAgeWorkflow' calls
-- to the stubbed activity implementation defined below.
workerConfig :: WorkerConfig ()
workerConfig = provideCallStack $ Worker.configure () definitions settings
  where
    definitions = (activity, workflow)
    settings = do
      Worker.setNamespace Shared.namespace
      Worker.setTaskQueue Shared.taskQueue
      Worker.setLogger (defaultOutput stdout)

-- | Manually construct the 'ProvidedWorkflow', since Template Haskell
-- discovery would automatically pick up the real activity implementation and
-- not the mock.
workflow :: ProvidedWorkflow (Text -> Workflow Text)
workflow = provideWorkflow JSON name estimateAgeWorkflow
  where
    name = knownWorkflowName (workflowRef EstimateAgeWorkflow)

-- | Manually construct a 'ProvidedActivity' implementation that conforms to
-- the same interface that 'EstimateAgeWorkflow' expects, but stubs out the
-- actual 'Activity' call and elides the custom HTTP client manager we provide
-- in the real implementation's execution environment.
activity :: ProvidedActivity () (Text -> Activity () Int)
activity = provideActivity JSON name (\_ -> pure 78)
  where
    name = knownActivityName (activityRef EstimateAge)
