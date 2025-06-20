module Main where

import Temporal.Activity
import Temporal.Exception (ApplicationFailure (..))
import Temporal.Testing.MockActivityEnvironment
import Test.Hspec
import Test.Hspec.Expectations
import Workflow

main :: IO ()
main = hspec $ do
  describe "translation activity" $ do
    it "successfully translates 'hello' to German" $ do
      mockEnv <- mkMockActivityEnvironment ()
      let input = TranslateTermInput "Hello" "de"
      res <- runMockActivity mockEnv $ translateActivity input
      res `shouldBe` "hallo"

    -- TODO: test that "Goodbye" gets successfully translated from
    -- Latvian

    -- TODO: test that an unrecognized language code fails with a 400
