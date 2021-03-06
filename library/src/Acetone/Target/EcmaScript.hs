{-# LANGUAGE MagicHash #-}
{-# LANGUAGE OverloadedStrings #-}

-- |
-- ECMAScript target for Acetone.
module Acetone.Target.EcmaScript
  ( -- * Infrastructure
    M

    -- * Names
  , fromGlobal
  , fromLocal

    -- * Units
  , fromUnit

    -- * Expressions
  , fromAnf
  , fromAction
  , fromValue

    -- * Intrinsics
  , fromIntrinsic
  ) where

import Acetone.Ir
import Acetone.Ir.Intrinsic

import Control.Lens (ifor_)
import Control.Monad (when)
import Data.ByteString.Builder (Builder)
import Data.Foldable (traverse_)

import qualified Data.ByteString.Builder as BB

--------------------------------------------------------------------------------
-- Infrastructure

-- |
-- Writer.
type M = (,) Builder

--------------------------------------------------------------------------------
-- Names

-- |
-- Translate a global to an ECMAScript identifier.
fromGlobal :: Global -> M ()
fromGlobal (Global g) = ("AG." <> BB.byteString g, ())

-- |
-- Translate a local to an ECMAScript identifier.
fromLocal :: Local -> M ()
fromLocal (Local l) = ("AL" <> BB.string7 (show l), ())

--------------------------------------------------------------------------------
-- Units

-- |
-- Translate a unit to an ECMAScript expression.
fromUnit :: Unit -> Builder
fromUnit unit = fst $ do
  tell "// Begin of Acetone-generated code.\n"
  tell "(function() {\n"
  tell "\"use strict\";\n"

  ifor_ unit $ \global body -> do
    fromGlobal global
    tell " = (function() {\n"

    result <- fromAnf body
    tell "return "
    fromValue result
    tell ";\n"

    tell "})();\n"

  tell "})();\n"
  tell "// End of Acetone-generated code.\n"

--------------------------------------------------------------------------------
-- Expressions

-- |
-- Translate an expression to a series of ECMAScript statements. The result of
-- the expression is not written; it is returned.
fromAnf :: Anf -> M Value
fromAnf (Anf actions result) = do
  traverse_ (uncurry fromAction) actions
  pure result

-- |
-- Translate an expression to series of ECMAScript statements. The result of
-- the expression is assigned to the given local.
fromAction :: Local -> Action -> M ()

fromAction local (ClosureAction parameters body) = do
  tell "var "
  fromLocal local
  tell " = function("

  ifor_ parameters $ \i parameter -> do
    when (i /= 0) $
      tell ", "
    fromLocal parameter

  tell ") {\n"

  result <- fromAnf body
  tell "return "
  fromValue result
  tell ";\n"

  tell "};\n"

fromAction local (IntrinsicAction intrinsic) =
  fromIntrinsic local (fromValue <$> intrinsic)

-- |
-- Translate a value to an ECMAScript expression.
fromValue :: Value -> M ()
fromValue (GlobalValue g) = fromGlobal g
fromValue (LocalValue g) = fromLocal g

--------------------------------------------------------------------------------
-- Intrinsics

-- |
-- Translate an intrinsic to series of ECMAScript statements. The result of the
-- intrinsic is assigned to the given local.
fromIntrinsic :: Local -> Intrinsic (M ()) -> M ()

fromIntrinsic local (Call# callee arguments) = do
  tell "var "
  fromLocal local
  tell " = "

  callee

  tell "("

  ifor_ arguments $ \i argument -> do
    when (i /= 0) $
      tell ", "
    argument

  tell ");\n"

fromIntrinsic local (Lazy# thunk) = do
  tell "var "
  fromLocal local
  tell " = AR.lazy("
  thunk
  tell ");\n"

fromIntrinsic local (Panic# message) = do
  -- While the AR.panic function never returns, we nonetheless declare a
  -- variable for its result, in the hope that the ECMAScript compiler will
  -- generate faster code because the variable is local when read.
  tell "var "
  fromLocal local
  tell ";\n"
  tell "AR.panic("
  message
  tell ");\n"

fromIntrinsic local (IntAdd# I32 left right) = do
  tell "var "
  fromLocal local
  tell " = "
  left
  tell " + "
  right
  tell " | 0;\n"

fromIntrinsic local (IntMul# I32 left right) = do
  tell "var "
  fromLocal local
  tell " = Math.imul("
  left
  tell ", "
  right
  tell ");\n"

fromIntrinsic local (EffectPure# value) = do
  tell "var "
  fromLocal local
  tell " = AR.effectPure("
  value
  tell ");\n"

fromIntrinsic local (EffectBind# action kleisli) = do
  tell "var "
  fromLocal local
  tell " = AR.effectBind("
  action
  tell ", "
  kleisli
  tell ");\n"

--------------------------------------------------------------------------------
-- Miscellaneous

tell :: Builder -> M ()
tell = flip (,) ()
