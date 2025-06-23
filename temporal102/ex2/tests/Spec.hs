module Main where

import qualified ActivitySpec
import qualified WorkflowSpec

import Test.Hspec

main :: IO ()
main = hspec do
  ActivitySpec.spec
  WorkflowSpec.spec

