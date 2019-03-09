{-# LANGUAGE OverloadedStrings #-}

module Acetone.Ir.Parse
  ( -- * Names
    global
  , local

    -- * Units
  , unit

    -- * Expressions
  , anf
  , action
  , value
  ) where

import Acetone.Ir

import Control.Applicative ((<|>), many)
import Data.Attoparsec.ByteString (Parser)
import Data.ByteString (ByteString)
import Data.Functor (void)
import Text.Read (readEither)

import qualified Data.Attoparsec.ByteString as P
import qualified Data.Attoparsec.ByteString.Char8 as P8
import qualified Data.ByteString.Char8 as B8
import qualified Data.Map as Map

--------------------------------------------------------------------------------
-- Names

global :: Parser Global
global = token $ do
  _ <- P8.char '@'
  n <- P8.takeWhile1 P8.isDigit >>= readResult
  pure (Global n)

local :: Parser Local
local = token $ do
  _ <- P8.char '%'
  n <- P8.takeWhile1 P8.isDigit >>= readResult
  pure (Local n)

--------------------------------------------------------------------------------
-- Units

unit :: Parser Unit
unit = P8.skipSpace *> go <* P.endOfInput where
  go = fmap Map.fromList $ do
         many $ do
           keyword "define"
           name <- global
           body <- anf
           pure (name, body)

--------------------------------------------------------------------------------
-- Expressions

anf :: Parser Anf
anf = do
  keyword "{"

  actions <- many $ do
    binding <- local
    keyword "="
    expression <- action
    pure (binding, expression)

  keyword "yield"
  result <- value

  keyword "}"

  pure (Anf actions result)

action :: Parser Action
action = callAction <|> closureAction
  where
  callAction    = do
    keyword "call"
    callee <- value
    argument <- value
    pure (CallAction callee argument)

  closureAction = do
    keyword "closure"
    parameter <- local
    body <- anf
    pure (ClosureAction parameter body)

value :: Parser Value
value = globalValue <|> localValue
  where
  globalValue = GlobalValue <$> global
  localValue  = LocalValue <$> local

--------------------------------------------------------------------------------
-- Miscellaneous

token :: Parser a -> Parser a
token p = p <* P8.skipSpace

keyword :: ByteString -> Parser ()
keyword = void . token . P8.string

readResult :: Read a => ByteString -> Parser a
readResult = either fail pure . readEither . B8.unpack
