module ActivitySpec (spec) where

import Workflow (estimateAge)

import TestUtils

import Network.HTTP.Client qualified as HTTP.Client
import Test.Hspec
import Test.Hspec.Expectations
import Temporal.Testing.MockActivityEnvironment (MockActivityEnvironment, runMockActivity)

-- | Prepare a mock environment and provide it to the activities under test.
spec :: Spec
spec = aroundAll withMockActivityEnvironment $
  activitySpec

-- | Test the age estimation activity in a mocked environment.
activitySpec :: SpecWith (MockActivityEnvironment HTTP.Client.Manager)
activitySpec = describe "age estimation activity" do
  it "runs the 'estimateAge' activity in a mocked environment" \env -> do
    age <- runMockActivity env $ estimateAge "Betty"
    age `shouldBe` 78
