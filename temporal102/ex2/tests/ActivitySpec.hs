module ActivitySpec where

import TestUtils

import Test.Hspec
import Test.Hspec.Expectations
import Temporal.Testing.MockActivityEnvironment (mkMockActivityEnvironment, runMockActivity)
import Workflow (TranslateTermInput(..), translateActivity)

-- | Test the translation activity in a mocked environment.
spec :: Spec
spec = describe "translation activity" do
  it "successfully translates 'hello' to German" do
    mockEnv <- mkMockActivityEnvironment ()
    let input = TranslateTermInput "Hello" "de"
    res <- runMockActivity mockEnv $ translateActivity input
    res `shouldBe` "hallo"
  -- FIXME: Needs to be implemented (Part B)
  it "successfully translates 'goodbye' to Latvian" do
    pending
  -- FIXME: Needs to be implemented (Part C)
  it "fails to translate with bad language code" do
    pending
