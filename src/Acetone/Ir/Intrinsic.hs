{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE MagicHash #-}

module Acetone.Ir.Intrinsic
  ( -- * Intrinsics
    Intrinsic (..)

    -- * Integers
  , IntSize (..)

    -- * Generators
  , genIntrinsic
  , genIntSize
  ) where

import Test.QuickCheck.Gen (Gen)

import qualified Test.QuickCheck.Gen as Gen

--------------------------------------------------------------------------------
-- Intrinsics

data Intrinsic a

  = Call# a [a]
  | Lazy# a

  | Panic# a

  | IntAdd# IntSize a a
  | IntMul# IntSize a a

  deriving stock (Eq, Foldable, Functor, Show, Traversable)

--------------------------------------------------------------------------------
-- Integers

data IntSize = I8 | I16 | I32 | I64
  deriving stock (Bounded, Enum, Eq, Show)

--------------------------------------------------------------------------------
-- Generators

genIntrinsic :: Gen a -> Gen (Intrinsic a)
genIntrinsic g = Gen.oneof
  [ Call# <$> g <*> Gen.listOf g
  , Lazy# <$> g

  , Panic# <$> g

  , IntAdd# <$> genIntSize <*> g <*> g
  , IntMul# <$> genIntSize <*> g <*> g ]

genIntSize :: Gen IntSize
genIntSize = Gen.oneof (fmap pure [minBound .. maxBound])
