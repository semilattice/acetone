module Acetone.Main.Test
  ( main
  ) where

import Test.Hspec (hspec)

import qualified Acetone.Optimize.DseSpec

main :: IO ()
main = hspec $ do
  Acetone.Optimize.DseSpec.spec
