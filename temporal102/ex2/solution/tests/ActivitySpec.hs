module ActivitySpec where

import TestUtils

import Data.Text (isInfixOf)
import Test.Hspec
import Test.Hspec.Expectations
import Temporal.Exception (ApplicationFailure(..))
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

  it "successfully translates 'goodbye' to Latvian" do
    mockEnv <- mkMockActivityEnvironment ()
    let input = TranslateTermInput "Goodbye" "lv"
    res <- runMockActivity mockEnv $ translateActivity input
    res `shouldBe` "ardievu"
    
  it "fails to translate with bad language code" do
    mockEnv <- mkMockActivityEnvironment ()
    let input = TranslateTermInput "Hello" "xq"
    let act = runMockActivity mockEnv $ translateActivity input
    act `shouldThrow` 
      (\e -> case e of
               ApplicationFailure "TranslationError" m _ _ _ _ -> "400" `isInfixOf` m
               _ -> False
      )
