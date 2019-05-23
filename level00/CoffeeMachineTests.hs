{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}
module CoffeeMachineTests (propertyTests) where

import Control.Applicative (liftA2)
import Control.Lens (Prism', matching, preview, review)
import Control.Lens.TH (makePrisms)

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.Hedgehog (testProperty)

import Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range


addTen :: Int -> Int
addTen = (+10)

prop_addTen :: Property
prop_addTen = property $ do
  n <- forAll $ Gen.int (Range.linear 0 1000)
  addTen n === n + 10





-- This function is broken, run the tests and see the failure output, then use
-- that information to fix this function.
badReverse :: [a] -> [a]
badReverse [] = []
badReverse (_:xs) = reverse xs

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
validCoin (Coin c) = c >= 0 && c <= maxCoinValue

-- This is the function we're going to write some tests for.
addCoins :: Coin -> Coin -> Maybe Coin
addCoins (Coin a) (Coin b) = 
  if a + b <= maxCoinValue
    then Just (Coin $ a + b)
    else Nothing

-- Write our generator for Coin
genCoin :: MonadGen m => m Coin
genCoin = Coin <$> Gen.int (Range.linear 0 maxCoinValue)

-- Test our 'normal' case, aka the happy path
prop_addCoins_Normal :: Property
prop_addCoins_Normal = property $ do
  Coin a <- forAll genCoin
  b <- forAll $ Gen.element [0, maxCoinValue - a]
  (Coin a) `addCoins` (Coin b) === Just (Coin $ a + b)

-- Test the 'overflow' case, aka the sad path
prop_addCoins_Overflow :: Property
prop_addCoins_Overflow = property $ do
  Coin a <- forAll genCoin
  b <- forAll $ Gen.element [maxCoinValue - a + 1, maxCoinValue]
  (Coin a) `addCoins` (Coin b) === Nothing

-- Instead of having separate properties, we can combine them into a single property test.
prop_addCoins :: Property
prop_addCoins = property $ do
  ( ca@(Coin a), cb@(Coin b) ) <- forAll $ liftA2 (,) genCoin genCoin

  let c = Coin (a + b)

  addCoins ca cb === if validCoin c 
    then Just c 
    else Nothing






-- Recursive structures
data MyTree a
  = Leaf a
  | Node (MyTree a) (MyTree a)
  deriving (Show, Eq)
$(makePrisms ''MyTree)

-- Fun generator to write, requires documentation diving (too much?)
genMyTree :: Gen a -> Gen (MyTree a)
genMyTree g = Gen.recursive Gen.choice 
  [ Leaf <$> g ]
  [ Node <$> genMyTree g <*> genMyTree g ]

-- Probably worth leaving as exercise for the reader as probably a bit silly for
-- workshop needs... :<

-- First, if I re or review a value with a Prism and then preview or use (^?),
-- I will get it back:
-- 
-- preview l (review l b) ≡ Just b
-- 
firstPrismLaw :: (Eq b, Show b) => Gen b -> Prism' a b -> Property
firstPrismLaw genB l = property $ forAll genB >>= \b ->
  preview l (review l b) === Just b

-- Second, if you can extract a value a using a Prism l from a value s, then
-- the value s is completely described by l and a:
-- 
-- preview l s ≡ Just a ==> review l a ≡ s
secondPrismLaw :: (Eq s, Show s) => Gen s -> Prism' s a -> Property
secondPrismLaw genS l = property $ forAll genS >>= \s ->
  case preview l s of
    Just a -> review l a === s
    _ -> success -- Not a matching value, disregard

-- Third, if you get non-match t, you can convert it result back to s:
-- 
-- matching l s ≡ Left t ==> matching l t ≡ Left s
thirdPrismLaw :: (Eq a, Show a, Eq s, Show s) => Gen s -> Prism' s a -> Property
thirdPrismLaw genS l = property $ forAll genS >>= \s ->
  case matching l s of
    Left t -> matching l t === Left s
    Right _ -> success  -- Not a matching value, disregard


-- Helper function, I don't think I would expect participants to write this. But
-- I probably wouldn't include it as it's just where my head went.
prop_myTreePrisms :: TestTree
prop_myTreePrisms = testGroup "MyTree prisms satisfy prism laws"
  [ mkLawProps "_Node" gTree ((,) <$> gTree <*> gTree) _Node
  , mkLawProps "_Leaf" gTree Gen.bool _Leaf
  ]
  where
    gTree = genMyTree Gen.bool

    mkLawProps :: ( Eq a, Show a , Eq s, Show s ) => String -> Gen s -> Gen a -> Prism' s a -> TestTree
    mkLawProps t genS genB p = testGroup (t <> " prism")
      [ testProperty "First law" $ firstPrismLaw genB p
      , testProperty "Second law" $ secondPrismLaw genS p
      , testProperty "Third law" $ thirdPrismLaw genS p
      ]






propertyTests :: TestTree
propertyTests = testGroup "Level00 - Property Tests"
  [ testProperty "Addition still works" prop_addTen
  , testProperty "Bad reverse is bad" prop_badReverse
  , prop_myTreePrisms
  , testProperty "Add Coins" prop_addCoins
  , testProperty "Add Coins (Normal)" prop_addCoins_Normal
  , testProperty "Add Coins (Overflow)" prop_addCoins_Overflow
  ]
