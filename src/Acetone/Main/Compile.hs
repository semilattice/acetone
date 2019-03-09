module Acetone.Main.Compile
  ( main
  ) where

import Acetone.Ir.Parse (unit)
import Data.Attoparsec.ByteString.Lazy (parse)

import qualified Data.ByteString.Lazy as BL

main :: IO ()
main = print . parse unit =<< BL.getContents
