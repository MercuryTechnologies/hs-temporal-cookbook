module WorkflowSpec where

import Shared qualified
import Workflow (EstimateAgeWorkflow (..))

import TestUtils

import Control.Monad.Trans.Reader (runReaderT)
import Test.Hspec
import Test.Hspec.Expectations
import Temporal.Client (WorkflowClient)
import qualified Temporal.Client as Client
import Temporal.Worker (WorkerConfig)

-- | Prepare the local dev server & worker process on a free port, then pass
-- a 'WorkflowClient' that can communicate with them to the test suite.
spec :: Spec
spec =
  aroundAll withDevServer
    . aroundAllWith (withWorker $ Shared.mkWorkerConfig globalRelaxedEMSManager)
    . aroundAllWith (withTestClient Shared.namespace)
    $ workflowSpec

-- | Test the age estimation workflow using the local dev server.
workflowSpec :: SpecWith WorkflowClient
workflowSpec = describe "EstimateAge workflow" do
  it "runs estimateAgeWorkflow with activity call" \client -> do
    message <- flip runReaderT client do
      Client.execute
        EstimateAgeWorkflow
        "test"
        (Client.startWorkflowOptions Shared.taskQueue)
        "Betty"
    message `shouldBe` "Betty has an estimated age of 78"
