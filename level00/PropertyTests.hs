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

addTen :: Int -> Int
addTen = (+10)

prop_addTen :: Property
prop_addTen = error "prop_addTen not implemented"
-- prop_addTen = property $ do
--   n <- forAll $ Gen.int (Range.linear 0 1000)
--   addTen n === n + 10





-- This function is broken, run the tests and see the failure output, then use
-- that information to fix this function.
badReverse :: [a] -> [a]
badReverse []     = []
badReverse (x:xs) = x : reverse xs

prop_badReverse :: Property
prop_badReverse = property $ do
  xs <- forAll (Gen.list (Range.linear 0 1000) Gen.bool)
  -- (optional): Are there other properties for reversing a list that could make
  -- this test more robust?
  badReverse (badReverse xs) === xs






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
genCoin = error "genCoin not implemented" --Coin <$> Gen.int (Range.linear 0 maxCoinValue)

-- Test our 'normal' case, aka the happy path
prop_addCoins_Normal :: Property
prop_addCoins_Normal = error "prop_addCoins_Normal not implemented"
  -- Coin a <- forAll genCoin
  -- b <- forAll $ Gen.element [0, maxCoinValue - a]
  -- (Coin a) `addCoins` (Coin b) === Just (Coin $ a + b)

-- Test the 'overflow' case, aka the sad path
prop_addCoins_Overflow :: Property
prop_addCoins_Overflow = error "prop_addCoins_Overflow not implemented"

  -- Coin a <- forAll genCoin
  -- b <- forAll $ Gen.element [maxCoinValue - a + 1, maxCoinValue]
  -- (Coin a) `addCoins` (Coin b) === Nothing

-- Instead of having separate properties, we can combine them into a single property test.
prop_addCoins_Combined :: Property
prop_addCoins_Combined = error "prop_addCoins not implemented"
  -- ( ca@(Coin a), cb@(Coin b) ) <- forAll $ liftA2 (,) genCoin genCoin
  -- let c = Coin (a + b)
  -- addCoins ca cb === if validCoin c
  --   then Just c
  --   else Nothing





genMyBTreeVal :: MonadGen m => m (Int, Char)
genMyBTreeVal = liftA2 (,) (Gen.int (Range.linear (-100) 100)) (Gen.enum 'a' 'z')

genTree :: MonadGen m => m (MyBTree Int Char)
genTree = fromList <$> Gen.list (Range.linear 0 100) genMyBTreeVal

prop_MyBTree_Insert :: Property
prop_MyBTree_Insert = property $ do
  tree <- forAll genTree
  newKV <- forAll genMyBTreeVal

  let
    pre            = toListWithKey tree
    treePostInsert = uncurry insert newKV tree
    expectation    = toListWithKey treePostInsert

    model = nubBy ((==) `on` fst)
      $ insertBy (compare `on` fst) newKV pre

  model === expectation

prop_MyBTree_Delete :: Property
prop_MyBTree_Delete = property $ do
  tree <- forAll genTree
  dKey <- forAll $ if null tree
    then pure 1
    else Gen.element (fst <$> toListWithKey tree)

  let
    pre            = toListWithKey tree
    treePostDelete = deleteKey dKey tree
    expectation    = toListWithKey treePostDelete

    model =
      filter ((/= dKey) . fst) pre

  model === expectation

propertyTests :: TestTree
propertyTests = testGroup "Level00 - Property Tests"
  [ testProperty "Addition still works" prop_addTen
  , testProperty "Bad reverse is bad" prop_badReverse
  , testProperty "Add Coins (Normal)" prop_addCoins_Normal
  , testProperty "Add Coins (Overflow)" prop_addCoins_Overflow
  , testProperty "Add Coins (Combined)" prop_addCoins_Combined

  , testProperty "BST insert" prop_MyBTree_Insert
  , testProperty "BST delete" prop_MyBTree_Delete
  ]
