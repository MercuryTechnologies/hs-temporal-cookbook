module Main where

import ActivitySpec qualified
import WorkflowSpec qualified

import Test.Hspec

main :: IO ()
main = hspec do
  ActivitySpec.spec
  WorkflowSpec.spec
