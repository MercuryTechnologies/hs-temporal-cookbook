module Main where

import WorkflowSpec qualified

import Test.Hspec

main :: IO ()
main = hspec do
  WorkflowSpec.spec
