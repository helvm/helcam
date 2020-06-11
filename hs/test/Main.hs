module Main where

import qualified Spec
import Test.Hspec.Slow
import Test.Hspec (hspec)

main :: IO ()
main = do
  config <- configure 1
  hspec $ timeThese config Spec.spec
