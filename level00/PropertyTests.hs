{-# OPTIONS_GHC -Wno-unused-binds -Wno-unused-imports #-}
{-# LANGUAGE RankNTypes #-}
module PropertyTests (propertyTests) where

import           Control.Applicative (liftA2)
import           Control.Lens        (Prism', matching, preview, review)
import           Control.Lens.TH     (makePrisms)

import           Data.Function       (on)
import           Data.List           (filter, insertBy, nubBy)

import           Test.Tasty          (TestTree, testGroup)
import           Test.Tasty.Hedgehog (testProperty)

import           Hedgehog
import qualified Hedgehog.Gen        as Gen
import qualified Hedgehog.Range      as Range

import           MyBTree

-- [OPTIONAL]
import           LawPropertiesBonus  (myBTreePrismLaws)

----------------------------------------------------------------------------------------------------
addTen :: Int -> Int
addTen = (+10)

-- A simple property for the 'addTen' function, ensure that:
--
-- \/ (x : Int) -> addTen x === (x + 10)
prop_addTen :: Property
prop_addTen = error "prop_addTen not implemented"

----------------------------------------------------------------------------------------------------

-- Property based testing is useful for debugging. In this exercise we've given
-- you a broken function. Using the output of the test, fix up the function. If
-- you're keen you can expand the properties that it must satisfy to make the
-- test, and commensurately the function itself, more robust.
--
badReverse :: [a] -> [a]
badReverse []     = []
badReverse (x:xs) = x : reverse xs

prop_badReverse :: Property
prop_badReverse = property $ do
  xs <- forAll (Gen.list (Range.linear 0 1000) Gen.bool)
  badReverse (badReverse xs) === xs
  -- [BONUS]: Are there other properties for reversing a list that could make this test more robust?

----------------------------------------------------------------------------------------------------

-- In this set of exercises we will build properties to ensure that our
-- functions for a given data structure do what we expect.

-- We will start with the following data structure.
newtype Coin = Coin Int deriving (Eq, Show)

-- That has a pre-determined maximum value.
maxCoinValue :: Int
maxCoinValue = 1000000

-- We're able to validate whether the Coin is valid.
validCoin :: Coin -> Bool
validCoin (Coin c) = c >= 0 && c < maxCoinValue

-- This is the function we're going to write some tests for.
addCoins :: Coin -> Coin -> Maybe Coin
addCoins (Coin a) (Coin b) =
  if a + b < maxCoinValue
    then Just (Coin $ a + b)
    else Nothing

-- Write our generator for Coin
genCoin :: MonadGen m => m Coin
genCoin = error "genCoin not implemented"

-- Test our 'normal' case, aka the happy path
prop_addCoins_Normal :: Property
prop_addCoins_Normal = error "prop_addCoins_Normal not implemented"

-- Test the 'overflow' case, aka the sad path
prop_addCoins_Overflow :: Property
prop_addCoins_Overflow = error "prop_addCoins_Overflow not implemented"

-- Instead of having separate properties, we can combine them into a single
-- property test.
prop_addCoins_Combined :: Property
prop_addCoins_Combined = error "prop_addCoins not implemented"

----------------------------------------------------------------------------------------------------

-- Using the binary search tree that is defined in the MyBTree module, then
-- complete the following functions.

-- To test our assumptions, we'll need to generate random MyBTrees. Using the
-- constructor functions from the MyBTree module, write a generator that can use
-- a given generator to populate the tree.
genTree :: (Ord k, MonadGen m) => m (k,v) -> m (MyBTree k v)
genTree genKV = fromList <$> Gen.list (Range.linear 0 100) genKV

-- To populate our tree, we need to generate some keys and their respective
-- values. We will make Hedgehog do this for us by reusing some of the built-in
-- generators.
genMyBTreeVal :: MonadGen m => m (Int, Char)
genMyBTreeVal = liftA2 (,) (Gen.int (Range.linear (-100) 100)) (Gen.enum 'a' 'z')

-- We're not ready to write a property test for inserting values into our binary
-- search tree.

-- To do this we need a 'model' that we know is correct to validate our
-- assumptions for the binary search tree. The simpler the better so we can be
-- confident that our representation is correct.
--
-- We're going to use an ordered list of pairs. Since it is a trivial data
-- structure and manipulating it is straightforward. We can be confident that
-- it is a good model for operations on our binary search tree.
--
-- fromList [(1, 'a'), (3,'c')] -> insert 2 'b' -> fromList [(1, 'a'), (2,'b'), (3,'c')]
--                   |                                            |
--                   |                                            |
--                   v                                            v
--          [(1, 'a'), (3,'c')] -> "insert" (2,'b') -> [(1, 'a'), (2,'b'), (3,'c')]
--
prop_MyBTree_Insert :: Property
prop_MyBTree_Insert = error "prop_MyBTree_Insert not implemented"

-- Now implement a test to ensure that we're correctly deleting elements within the tree.
prop_MyBTree_Delete :: Property
prop_MyBTree_Delete = error "prop_MyBTree_Delete not implemented"

----------------------------------------------------------------------------------------------------
  --
propertyTests :: TestTree
propertyTests = testGroup "Level00 - Property Tests"
  [ testProperty "Addition still works" prop_addTen
  , testProperty "Bad reverse is bad" prop_badReverse
  , testProperty "Add Coins (Normal)" prop_addCoins_Normal
  , testProperty "Add Coins (Overflow)" prop_addCoins_Overflow
  , testProperty "Add Coins (Combined)" prop_addCoins_Combined

  , testProperty "BST insert" prop_MyBTree_Insert
  , testProperty "BST delete" prop_MyBTree_Delete

  -- OPTIONAL, exercises for this property are in 'LawPropertiesBonus.hs'
  --
  -- These implementing prisms and writing property tests to validate that the
  -- given prism is law abiding. It is an optional exercise for your own
  -- enjoyment and to demonstrate that property based testing can provide
  -- _immense_ power for validating necessary assumptions.
  --
  -- , testProperty "Prism Laws Hold for MyBTree" myBTreePrismLaws
  ]
