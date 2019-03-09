{-# LANGUAGE OverloadedLists #-}

-- |
-- Dead store elimination: remove actions whose results are unused.
module Acetone.Optimize.Dse
  ( onUnit
  , onAnf
  ) where

import Acetone.Ir

import Prelude hiding (init)

import Control.Lens (transform)
import Control.Parallel.Strategies (Strategy, parTraversable, using)

import qualified Data.Set as Set

-- |
-- Apply the optimization to all expressions in a unit, possibly in parallel.
onUnit :: Strategy Anf -> Unit -> Unit
onUnit s u = fmap onAnf u `using` parTraversable s

-- |
-- Apply the optimization to an expression and its subexpressions.
onAnf :: Anf -> Anf
onAnf = transform $
          filterActionsWithFree $
            \locals local _ ->
              local `Set.member` locals
