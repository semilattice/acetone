{-# LANGUAGE MagicHash #-}

module Acetone.Optimize.DseSpec
  ( spec
  ) where

import Acetone.Ir
import Acetone.Ir.Intrinsic
import Acetone.Optimize.Dse

import Test.Hspec (Spec, describe, shouldBe, shouldSatisfy)
import Test.Hspec.QuickCheck (prop)
import Test.QuickCheck ((==>), forAll)

import qualified Data.Set as Set

spec :: Spec
spec = describe "Adrenaline.Optimize.Dse" $ do

  prop "all used actions are retained" $
    forAll genValue $ \value1 ->
    forAll genValue $ \value2 ->
    forAll genLocal $ \result ->
      let input  = Anf [(result, IntrinsicAction (Call# value1 value2))]
                       (LocalValue result) in
      let output = input in
      onAnf input `shouldBe` output

  prop "no unused actions are retained" $
    forAll genValue $ \value1 ->
    forAll genValue $ \value2 ->
    forAll genValue $ \value3 ->
    forAll genValue $ \value4 ->
    forAll genLocal $ \result ->
    forAll genLocal $ \redund ->
    result /= redund ==>
      let input  = Anf [ (result, IntrinsicAction (Call# value1 value2))
                       , (redund, IntrinsicAction (Call# value3 value4)) ]
                       (LocalValue result) in
      let output = Anf [(result, IntrinsicAction (Call# value1 value2))]
                       (LocalValue result) in
      onAnf input `shouldBe` output

  prop "no new free variables are introduced" $
    forAll genAnf $ \anf ->
      free (onAnf anf) `shouldSatisfy`
        (`Set.isSubsetOf` free anf)
