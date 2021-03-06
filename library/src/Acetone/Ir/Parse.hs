{-# LANGUAGE MagicHash #-}
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

    -- * Intrinsics
  , intrinsic
  , intSize
  ) where

import Acetone.Ir
import Acetone.Ir.Intrinsic

import Control.Applicative ((<|>), liftA2, many)
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
  n <- P8.takeWhile1 (liftA2 (||) P8.isAlpha_ascii P8.isDigit)
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
    keyword ";"
    pure (binding, expression)

  keyword "yield"
  result <- value
  keyword ";"

  keyword "}"

  pure (Anf actions result)

action :: Parser Action
action = closureAction <|> intrinsicAction
  where

  closureAction = do
    keyword "closure"
    parameters <- many local
    body <- anf
    pure (ClosureAction parameters body)

  intrinsicAction =
    IntrinsicAction <$> intrinsic value

value :: Parser Value
value = globalValue <|> localValue
  where
  globalValue = GlobalValue <$> global
  localValue  = LocalValue <$> local

--------------------------------------------------------------------------------
-- Intrinsics

intrinsic :: Parser a -> Parser (Intrinsic a)
intrinsic p =
  call# <|> lazy# <|> panic# <|> intAdd# <|> intMul# <|> effectPure# <|>
  effectBind#
  where

  call# = do { keyword "call"; Call# <$> p <*> many p }
  lazy# = do { keyword "lazy"; Lazy# <$> p }

  panic# = do { keyword "panic"; Panic# <$> p }

  intAdd# = do { keyword "intAdd"; IntAdd# <$> intSize <*> p <*> p }
  intMul# = do { keyword "intMul"; IntMul# <$> intSize <*> p <*> p }

  effectPure# = do { keyword "effectPure"; EffectPure# <$> p }
  effectBind# = do { keyword "effectBind"; EffectBind# <$> p <*> p }

intSize :: Parser IntSize
intSize = i8 <|> i16 <|> i32 <|> i64
  where
  i8  = I8  <$ keyword "i8"
  i16 = I16 <$ keyword "i16"
  i32 = I32 <$ keyword "i32"
  i64 = I64 <$ keyword "i64"

--------------------------------------------------------------------------------
-- Miscellaneous

token :: Parser a -> Parser a
token p = p <* P8.skipSpace

keyword :: ByteString -> Parser ()
keyword = void . token . P8.string

readResult :: Read a => ByteString -> Parser a
readResult = either fail pure . readEither . B8.unpack
