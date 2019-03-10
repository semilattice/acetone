{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- |
-- Internal representation of programs.
module Acetone.Ir
  ( -- * Names
    Global (..)
  , Local (..)

    -- * Units
  , Unit

    -- * Expressions
  , Anf (..)
  , Action (..)
  , Value (..)

    -- * Free locals
  , HasFree (..)
  , filterActionsWithFree
  , filterAccumRActionsWithFree

    -- * Generators
  , genGlobal
  , genLocal
  , genAnf
  , genAction
  , genValue
  ) where

import Prelude hiding (init)

import Acetone.Ir.Intrinsic (Intrinsic, genIntrinsic)
import Control.Applicative (liftA2)
import Control.Lens -- (Plated (..), Traversal', (&), (^.), (.~), _2, at)
import Data.Map (Map)
import Data.Set (Set, (\\))
import Data.Word (Word64)
import Test.QuickCheck.Gen (Gen)

import qualified Data.Set as Set

import qualified Test.QuickCheck.Arbitrary as Gen
import qualified Test.QuickCheck.Gen as Gen

--------------------------------------------------------------------------------
-- Names

-- |
-- Name of a globally defined value.
newtype Global =
  Global Word64
  deriving stock (Eq, Ord, Show)

-- |
-- Name of a locally defined value.
newtype Local =
  Local Word64
  deriving stock (Eq, Ord, Show)

--------------------------------------------------------------------------------
-- Units

-- |
-- Collection of expressions.
type Unit =
  Map Global Anf

--------------------------------------------------------------------------------
-- Expressions

-- |
-- Expression that computes a value based on actions and values.
data Anf = Anf [(Local, Action)] Value
  deriving stock (Eq, Show)

-- |
-- Expression that computes a value based on values.
data Action
  = ClosureAction [Local] Anf
  | IntrinsicAction (Intrinsic Value)
  deriving stock (Eq, Show)

-- |
-- Value.
data Value
  = GlobalValue Global
  | LocalValue Local
  deriving stock (Eq, Show)

-- |
-- Traversal over 'ClosureAction' bodies and similar.
instance Plated Anf where
  plate k (Anf actions result) =
    Anf <$> traverse (traverse (go k)) actions <*> pure result
    where
    go :: Traversal' Action Anf
    go l (ClosureAction a b) = ClosureAction <$> pure a <*> l b
    go _ (IntrinsicAction a) = IntrinsicAction <$> pure a

--------------------------------------------------------------------------------
-- Free locals

class HasFree a where
  -- |
  -- Get all locals that appear free in an expression.
  free :: a -> Set Local

instance HasFree Anf where
  free (Anf actions result) =
    let
      init = free result
      step (local, action) locals =
        mappend (locals & at local .~ Nothing)
                (free action)
    in
      foldr step init actions

instance HasFree Action where
  free (ClosureAction a b) = free b \\ Set.fromList a
  free (IntrinsicAction a) = foldMap free a

instance HasFree Value where
  free (GlobalValue _) = []
  free (LocalValue a) = [a]

-- |
-- Filter the actions inside an expression. The predicate additionally receives
-- the free locals of the continuation for each action. The free locals given
-- to the predicate do not include those of actions that were removed by the
-- predicate.
filterActionsWithFree
  :: (Set Local -> Local -> Action -> Bool) -> Anf -> Anf
filterActionsWithFree p anf =
  filterAccumRActionsWithFree (\a () b c -> ((), p a b c)) () anf ^. _2

-- |
-- Like 'filterActionsWithFree', but thread through an additional accumulator.
filterAccumRActionsWithFree
  :: forall a. (Set Local -> a -> Local -> Action -> (a, Bool))
  -> a -> Anf -> (a, Anf)

filterAccumRActionsWithFree p z (Anf actions result) =

  let
    z' :: (Set Local, a, [(Local, Action)])
    z' = (free result, z, [])

    p' :: (Local, Action) -> (Set Local, a, [(Local, Action)])
       -> (Set Local, a, [(Local, Action)])
    p' (local, action) (used, acc, actions') =
      let (acc', keep) = p used acc local action in
      let used' = used & at local .~ Nothing in
      if keep then (used' <> free action, acc', (local, action) : actions')
              else (used', acc', actions')

  in
    let (_, acc, actions') = foldr p' z' actions in
    (acc, Anf actions' result)

--------------------------------------------------------------------------------
-- Generators

genGlobal :: Gen Global
genGlobal = Global <$> Gen.arbitrary

genLocal :: Gen Local
genLocal = Local <$> Gen.arbitrary

genAnf :: Gen Anf
genAnf = Anf <$> Gen.listOf (liftA2 (,) genLocal (genHalf genAction))
             <*> genValue

genAction :: Gen Action
genAction = Gen.oneof
  [ ClosureAction <$> Gen.listOf genLocal <*> genHalf genAnf
  , IntrinsicAction <$> genIntrinsic genValue ]

genValue :: Gen Value
genValue = Gen.oneof
  [ GlobalValue <$> genGlobal
  , LocalValue <$> genLocal ]

genHalf :: Gen a -> Gen a
genHalf = Gen.scale (`div` 2)
