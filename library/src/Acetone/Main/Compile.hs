{-# LANGUAGE LambdaCase #-}

module Acetone.Main.Compile
  ( main
  ) where

import Acetone.Ir.Parse (unit)
import Control.Lens ((&))
import Data.Attoparsec.ByteString (parseOnly)
import System.IO (stdout)

import qualified Acetone.Target.EcmaScript as EcmaScript
import qualified Data.ByteString as B
import qualified Data.ByteString.Builder as BB

main :: IO ()
main = do
  text <- B.getContents
  ir <- parseOnly unit text & either fail pure
  let ecmaScript = EcmaScript.fromUnit ir
  BB.hPutBuilder stdout ecmaScript
