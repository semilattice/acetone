{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ViewPatterns #-}

-- |
-- Monadic EDSL for generating ANF.
module Acetone.Ir.Generate
  ( -- * Infrastructure
    G
  , runG

    -- * Language
  , closureAction
  , closureAction0
  , closureAction1
  , closureAction2
  , intrinsicAction
  ) where

import Acetone.Ir

import Acetone.Ir.Intrinsic (Intrinsic)

import Control.Lens ((.=), (%=), (<<.=), (<<+=), makeLenses, use)
import Control.Monad (replicateM)
import Control.Monad.Trans.State (State, runState)
import Data.Word (Word64)

--------------------------------------------------------------------------------
-- Infrastructure

newtype G a =
  G (State S a)
  deriving newtype (Applicative, Functor, Monad)

data S =
  S
    { _sNextId  :: Word64
    , _sActions :: [(Local, Action)] }

$(makeLenses ''S)

runG :: G (Value, a) -> (Anf, a)
runG (G g) =
  let ((result, a), S _ (reverse -> actions)) = runState g (S 0 []) in
  (Anf actions result, a)

-- |
-- Run an action with the same 'sNextId' but with a new 'sActions'.
embedG :: G Value -> G Anf
embedG g = do
  outerActions <- G $ sActions <<.= []
  result <- g
  (reverse -> innerActions) <- G $ use sActions
  G $ sActions .= outerActions
  pure $ Anf innerActions result

freshLocal :: G Local
freshLocal = G . fmap Local $ sNextId <<+= 1

emitAction :: Action -> G Value
emitAction action = do
  local <- freshLocal
  G $ sActions %= ((local, action) :)
  pure $ LocalValue local

--------------------------------------------------------------------------------
-- Language

closureAction :: Int -> ([Local] -> G Value) -> G Value
closureAction arity body = do
  parameters <- replicateM arity freshLocal
  body' <- embedG (body parameters)
  emitAction $ ClosureAction parameters body'

closureAction0 :: G Value -> G Value
closureAction0 body =
  closureAction 0 $ \case
    [] -> body
    _  -> error "Blame the lack of dependent types."

closureAction1 :: (Local -> G Value) -> G Value
closureAction1 body = do
  closureAction 1 $ \case
    [a] -> body a
    _   -> error "Blame the lack of dependent types."

closureAction2 :: (Local -> Local -> G Value) -> G Value
closureAction2 body = do
  closureAction 2 $ \case
    [a, b] -> body a b
    _      -> error "Blame the lack of dependent types."

intrinsicAction :: Intrinsic Value -> G Value
intrinsicAction = emitAction . IntrinsicAction
