{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE TemplateHaskell #-}

module Epoxy.Check
  ( -- * Infrastructure
    Error (..)
  , CE
  , runCE

    -- * Polymorphism
  , instantiate
  , skolemize

    -- * Expressions
  , checkExpression
  ) where

import Epoxy.Ast

import Control.Lens ((?~), (.=), (?=), (<<%=), (<<+=), at, ix, makeLenses, preuse, rewrite, use)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.State (StateT, evalStateT)
import Data.Map (Map)
import Data.Word (Word64)

import qualified Data.Map as Map

--------------------------------------------------------------------------------
-- Infrastructure

data Error
  = Error
  deriving stock (Eq, Show)

type CE =
  StateT CES (Either Error)

data CES =
  CES
    { _cesNextId :: Word64
    , _cesScope  :: Map Identifier Scheme
    , _cesKnowns :: Map Unknown Type }

$(makeLenses ''CES)

runCE :: CE a -> Map Identifier Scheme -> Either Error a
runCE a γ = evalStateT a (CES 0 γ Map.empty)

withScope :: (Map Identifier Scheme -> Map Identifier Scheme) -> CE a -> CE a
withScope f a = do
  oldScope <- cesScope <<%= f
  result <- a
  cesScope .= oldScope
  pure result

freshUnknown :: CE Unknown
freshSkolem  :: CE Skolem
freshUnknown = fmap Unknown $ cesNextId <<+= 1
freshSkolem  = fmap Skolem  $ cesNextId <<+= 1

freshUnknownType, freshSkolemType :: CE Type
freshUnknownType = UnknownType <$> freshUnknown
freshSkolemType  = SkolemType  <$> freshSkolem

-- |
-- From now on, the unknown is known to be this type.
know :: Unknown -> Type -> CE ()
know u t =
  -- TODO: Occurs check.
  cesKnowns . at u ?= t

-- |
-- Remove outer unknowns when possible.
purge :: Type -> CE Type
purge t@(UnknownType u) = use (cesKnowns . at u) >>= maybe (pure t) purge
purge t = pure t

-- |
-- Make two types the same.
unify :: Type -> Type -> CE ()
unify a b = do
  a' <- purge a
  b' <- purge b
  unify' a' b'

unify' :: Type -> Type -> CE ()

unify' (UnknownType a) (UnknownType b) | a == b = pure ()
unify' (UnknownType a) b = know a b
unify' a (UnknownType b) = know b a

unify' (SkolemType a) (SkolemType b) | a == b = pure ()
unify' SkolemType{} _ = lift (Left Error)
unify' _ SkolemType{} = lift (Left Error)

unify' (VariableType a) (VariableType b) | a == b = pure ()
unify' VariableType{} _ = lift (Left Error)
unify' _ VariableType{} = lift (Left Error)

unify' (ApplicationType a b) (ApplicationType c d) = do
  unify a c
  unify b d

--------------------------------------------------------------------------------
-- Polymorphism

instantiate :: Scheme -> CE Type
instantiate = instantiateWith freshUnknownType

skolemize :: Scheme -> CE Type
skolemize = instantiateWith freshSkolemType

instantiateWith :: CE Type -> Scheme -> CE Type
instantiateWith fresh (Scheme forall type_) = do
  flip go type_ . Map.fromList <$>
    sequence [ (,) v <$> fresh | v <- forall ]
  where
  go :: Map Identifier Type -> Type -> Type
  go γ = rewrite $ \t -> case t of
    UnknownType{} -> Nothing
    SkolemType{} -> Nothing
    VariableType a -> Map.lookup a γ
    ApplicationType{} -> Nothing

--------------------------------------------------------------------------------
-- Expressions

checkExpression :: Expression -> CE Type

checkExpression (VariableExpression name) =
  preuse (cesScope . ix name) >>=
    maybe (lift (Left Error)) instantiate

checkExpression (ApplicationExpression callee argument) = do
  calleeT   <- checkExpression callee
  argumentT <- checkExpression argument
  resultT   <- freshUnknownType
  unify calleeT (argumentT :->: resultT)
  pure resultT

checkExpression (LambdaExpression parameter body) = do
  parameterT <- freshUnknownType
  parameterS <- pure $ Scheme [] parameterT
  bodyT      <- withScope (at parameter ?~ parameterS) $
                  checkExpression body
  pure (parameterT :->: bodyT)
