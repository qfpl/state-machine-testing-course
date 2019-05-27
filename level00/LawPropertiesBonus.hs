{-# OPTIONS_GHC -Wno-unused-binds -Wno-unused-imports #-}
{-# LANGUAGE RankNTypes #-}
module LawPropertiesBonus 
  ( myBTreePrismLaws
  , firstPrismLaw
  , secondPrismLaw
  , thirdPrismLaw
  , _Empty
  , _Node
  ) where

import           Control.Lens   (Prism', matching, preview, prism, review)

import           Hedgehog
import qualified Hedgehog.Gen   as Gen
import qualified Hedgehog.Range as Range

import           MyBTree        (MyBTree (..))

-- Complete the following prisms, then write properties that check the Prism laws hold.
_Empty :: Prism' (MyBTree k a) ()
_Empty = error "_Empty prism not implemented"

_Node :: Prism' (MyBTree k a) (MyBTree k a, (k, a), MyBTree k a)
_Node = error "_Node prism not implemented"

-- First, if I re or review a value with a Prism and then preview or use (^?),
-- I will get it back:
--
-- preview l (review l b) ≡ Just b
--
firstPrismLaw :: (Eq b, Show b) => Gen b -> Prism' a b -> Property
firstPrismLaw _gen _p = error "firstPrismLaw not implemented"

-- Second, if you can extract a value a using a Prism l from a value s, then
-- the value s is completely described by l and a:
--
-- preview l s ≡ Just a ==> review l a ≡ s
secondPrismLaw :: (Eq s, Show s) => Gen s -> Prism' s a -> Property
secondPrismLaw _gen _p = error "secondPrismLaw not implemented"

-- Third, if you get non-match t, you can convert it result back to s:
--
-- matching l s ≡ Left t ==> matching l t ≡ Left s
thirdPrismLaw :: (Eq a, Show a, Eq s, Show s) => Gen s -> Prism' s a -> Property
thirdPrismLaw _gen _p = error "thirdPrismLaw not implemented"

myBTreePrismLaws :: Property
myBTreePrismLaws = error "Law tests for MyBTree prisms not yet implemented"
