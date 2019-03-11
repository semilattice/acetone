{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}

module Epoxy.Ast
  ( -- * Names
    Identifier (..)
  , Unknown (..)
  , Skolem (..)

    -- * Units
  , Unit

    -- * Definitions
  , Definition (..)

    -- * Expressions
  , Expression (..)

    -- * Types
  , Scheme (..)
  , Type (..)

    -- * Constants
  , pattern FunctionType
  , pattern (:->:)
  ) where

import Control.Lens (Plated (..))
import Data.ByteString (ByteString)
import Data.Word (Word64)

--------------------------------------------------------------------------------
-- Names

newtype Identifier =
  Identifier ByteString
  deriving stock (Eq, Ord, Show)

newtype Unknown =
  Unknown Word64
  deriving stock (Eq, Ord, Show)

newtype Skolem =
  Skolem Word64
  deriving stock (Eq, Ord, Show)

--------------------------------------------------------------------------------
-- Units

type Unit =
  [Definition]

--------------------------------------------------------------------------------
-- Definitions

data Definition
  = ValueDefinition Identifier Scheme Expression
  deriving stock (Eq, Show)

--------------------------------------------------------------------------------
-- Expressions

data Expression
  = VariableExpression Identifier
  | ApplicationExpression Expression Expression
  | LambdaExpression Identifier Expression
  deriving stock (Eq, Show)

--------------------------------------------------------------------------------
-- Types

data Scheme =
  Scheme
    { schemeForall :: [Identifier]
    , schemeType   :: Type }
  deriving stock (Eq, Show)

data Type
  = UnknownType Unknown
  | SkolemType Skolem
  | VariableType Identifier
  | ApplicationType Type Type
  deriving stock (Eq, Show)

pattern FunctionType :: Type
pattern FunctionType = VariableType (Identifier "Function")

pattern (:->:) :: Type -> Type -> Type
pattern (:->:) a b = ApplicationType (ApplicationType FunctionType a) b

instance Plated Type where
  plate _ (UnknownType a) = UnknownType <$> pure a
  plate _ (SkolemType a) = SkolemType <$> pure a
  plate _ (VariableType a) = VariableType <$> pure a
  plate k (ApplicationType a b) = ApplicationType <$> k a <*> k b
