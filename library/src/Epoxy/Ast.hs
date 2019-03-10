{-# LANGUAGE DerivingStrategies #-}

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
  ) where

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
