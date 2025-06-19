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
    it "throws an exception when language code isn't recognized" $ do
      mockEnv <- mkMockActivityEnvironment ()
      let input = TranslateTermInput "Hello" "fi" -- hei!
      let act = runMockActivity mockEnv $ translateActivity input
      act `shouldThrow` 
        (\e -> case e of
                 ApplicationFailure "TranslationError" _ _ _ _ _ -> True
                 _ -> False
        )
