module Main where

import qualified ActivitySpec
import Test.Hspec

main :: IO ()
main = hspec do
  ActivitySpec.spec
