module Main where

import ActivitySpec qualified
import MockWorkflowSpec qualified
import WorkflowSpec qualified

import Test.Hspec

main :: IO ()
main = hspec do
  ActivitySpec.spec
  MockWorkflowSpec.spec
  WorkflowSpec.spec
