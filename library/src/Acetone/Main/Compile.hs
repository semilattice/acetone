{-# LANGUAGE LambdaCase #-}

module Acetone.Main.Compile
  ( main
  ) where

import Acetone.Ir.Parse (unit)
import Control.Lens ((&))
import Control.Parallel.Strategies (rpar)
import Data.Attoparsec.ByteString (parseOnly)
import System.IO (stdout)

import qualified Acetone.Optimize.Dse as Dse
import qualified Acetone.Target.EcmaScript as EcmaScript
import qualified Data.ByteString as B
import qualified Data.ByteString.Builder as BB

main :: IO ()
main = do
  text <- B.getContents
  ir <- parseOnly unit text & either fail pure
  let ir' = Dse.onUnit rpar ir
  let ecmaScript = EcmaScript.fromUnit ir'
  BB.hPutBuilder stdout ecmaScript
