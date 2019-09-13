{-# OPTIONS_GHC -fno-warn-orphans -Wno-unused-binds -Wno-unused-imports #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE RankNTypes   #-}
{-# LANGUAGE TypeApplications #-}
module PropertyTests (propertyTests) where

import           Control.Applicative (liftA2)
import           Control.Lens        (Prism', matching, preview, review)
import           Control.Lens.TH     (makePrisms)
import           Control.Monad       (when)

import           Data.Function       (on)
import           Data.List           (filter, insertBy, nubBy)
import           Data.Monoid         (Endo (..))

import           Test.Tasty          (TestTree, testGroup)
import           Test.Tasty.Hedgehog (testProperty)

import           Hedgehog
import qualified Hedgehog.Gen        as Gen
import qualified Hedgehog.Range      as Range
import qualified Hedgehog.Function   as Fn

import           MyBTree

-- [OPTIONAL]

----------------------------------------------------------------------------------------------------
addTen :: Int -> Int
addTen = appEndo . foldMap Endo $ replicate 10 succ

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
badReverse (_:xs) = reverse xs

prop_badReverse :: Property
prop_badReverse = property $ do
  xs <- forAll $ Gen.list (Range.linear 0 1000) Gen.bool
  badReverse (badReverse xs) === xs
  -- [BONUS]: Are there other properties for reversing a list that could make this test more robust?

----------------------------------------------------------------------------------------------------

-- In this set of exercises we will build properties to ensure that our
-- functions for a given data structure do what we expect.
--
-- These examples are lifted from a presentation by John Hughes: "Building on developer intuitions".
-- Which may be viewed at: https://www.youtube.com/watch?v=NcJOiQlzlXQ
--
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

----------------------------------NOTE-----------------------------------
-- If you're crusing through these exercises, jump straight to         --
-- 'level01' instead of working through absolutely everything. The     --
-- state machine testing is the focus of the course, after all!.       --
-------------------------------------------------------------------------

----------------------------------------------------------------------------------------------------

-- Use the binary search tree that is defined in the MyBTree module, to
-- complete the following functions.
--
-- These examples are lifted from a presentation by John Hughes: "Building on developer intuitions".
-- Which may be viewed at: https://www.youtube.com/watch?v=NcJOiQlzlXQ

-- To test our assumptions, we'll need to generate random MyBTrees.
-- Use the constructors from the MyBTree module and write a
-- generator for MyBTrees.
genTree0 :: (Ord k, MonadGen g) => g k -> g v -> g (MyBTree k v)
genTree0 genK genV = Gen.recursive Gen.choice
  -- Non-recursive component of our tree
  [ Gen.constant Empty ]
  -- Recursive component of our tree
  [ nodeGen ]
  where
    nodeGen = do
      k <- genK
      v <- genV
      let
        genLK = Gen.filter (< k) genK
        genRK = Gen.filter (> k) genK

      Gen.subterm2 (genTree0 genLK genV) (genTree0 genRK genV) (\l r -> Node l (k,v) r)

genTree :: (Ord k, MonadGen m) => (m k, m v) -> m (MyBTree k v)
genTree = uncurry genTree0

-- To populate our tree, we need to generate some keys and their respective
-- values. We will make Hedgehog do this for us by reusing some of the built-in
-- generators.
genMyBTreeVal :: MonadGen m => (m Int, m Char)
genMyBTreeVal = (Gen.int (Range.linear 0 1000), Gen.enum 'a' 'z')

-- Confirm that we're generating valid data
prop_tree_gen_valid :: Property
prop_tree_gen_valid = property $ forAll (genTree genMyBTreeVal) >>= assert . valid

-- We can also use property based testing to ensure that when we
-- implement a typeclass instance, that implementation will comply
-- with the laws of that typeclass. This is important as often the
-- laws of a typeclass cannot be expressed in the types.
--
-- The Eq instance from deriving Eq is too granular: trees with the
-- same content but different structure will be considered unequal,
-- which will be surprising to our callers.
--
-- Implement the 'Eq' instance for 'MyBTree' that compares for two
-- trees having a matching set of (key, value) pairs.
--
-- The properties and more info can be found in the documentation for the 'Eq' class on Hackage:
-- https://hackage.haskell.org/package/base/docs/Data-Eq.html#t:Eq
--
instance (Eq k, Eq a) => Eq (MyBTree k a) where
  (==) :: MyBTree k a -> MyBTree k a -> Bool
  (==) = error "Eq instance for MyBTree not yet implemented"

prop_MyBTree_LawfulEqInstance :: Property
prop_MyBTree_LawfulEqInstance = property $ do
  (x,y,z) <- forAll $ (,,) <$> genTree genMyBTreeVal <*> genTree genMyBTreeVal <*> genTree genMyBTreeVal

  annotate "Reflexivity: x == x = True"
  failure

  annotate "Symmetry: x == y = y == x"
  failure

  annotate "Transitivity: if x == y && y == x = True then x == z = True"
  failure

  annotate "Negation: x /= y = not (x == y)"
  failure

  -- An example of using 'hedgehog-fn' to generate functions to test
  -- substitutivity. We've includeded this rather than leave it as an
  -- exercise as function generation can be quite wild.
  --
  -- There is no 'Generic' instance for Char, Int however does have
  -- such an instance. So generate 'MyBTree Int Int' to satisfy the
  -- requirements for 'hedgehog-fn' function generation.
  let
    genKey = Gen.int (Range.linear (-100) 100)
    genVal = Gen.int (Range.linear 200 500)
    genIntTree = genTree0 genKey genVal

  (i,j) <- forAll $ liftA2 (,) genIntTree genIntTree
  -- This generates a function of the following type: 'g :: MyBTree Int Int -> Bool'
  g <- Fn.forAllFn $ Fn.fn @(MyBTree Int Int) Gen.bool

  annotate "Substitutivity: if x == y = True and g is a function whose return type is an instance of Eq, then g x == g y = True"
  when (i == j) $ (g i == g j) === True
  -----

----------------------------------NOTE-----------------------------------
-- If you're crusing through these exercises, jump straight to         --
-- 'level01' instead of working through absolutely everything. The     --
-- state machine testing is the focus of the course, after all!.       --
-------------------------------------------------------------------------

-- We're now ready to write a property test for inserting values into our binary
-- search tree.
--
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
--          [(1, 'a'), (3,'c')] -> modelInsert (2,'b') -> [(1, 'a'), (2,'b'), (3,'c')]
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
  [
    testProperty "Addition still works" prop_addTen
  , testProperty "Bad reverse is bad" prop_badReverse
  , testProperty "Add Coins (Normal)" prop_addCoins_Normal
  , testProperty "Add Coins (Overflow)" prop_addCoins_Overflow
  , testProperty "Add Coins (Combined)" prop_addCoins_Combined

  , testProperty "Lawful Eq instance for MyBTree" prop_MyBTree_LawfulEqInstance
  , testProperty "BST insert" prop_MyBTree_Insert
  , testProperty "BST delete" prop_MyBTree_Delete

  , testProperty "Using intended Eq instance implementation" prop_desired_eq_instance
  , testProperty "MyBTree generator is valid" prop_tree_gen_valid
  ]

----------------------------------------------------------------------
--                                                                  --
-- Extra test to make sure you're using the intended Eq instance,   --
-- other instances should cause this test to fail. Will need more   --
-- tests as participants become more tricksy, precious.             --
--                                                                  --
----------------------------------------------------------------------
prop_desired_eq_instance :: Property
prop_desired_eq_instance = withTests 1 . property $ do
  let
    kvs = [(1,'a'), (2, 'b'), (3, 'c')] :: [(Int,Char)]
    kvsDiffVal = [(1,'a'), (2, 'd'), (3, 'c')]
    kvsDiffKey = [(1,'a'), (2, 'b'), (5, 'c')]

    -- These will contain matching keys and values but will be structurally different.
    t1 = fromList kvs           -- Node Empty (1,'a') (Node Empty (2,'b') (Node Empty (3, 'c') Empty))
    t2 = fromList (reverse kvs) -- Node (Node (Node Empty (1,'a') Empty) (2, 'b') Empty) (3,'c') Empty
    t3 = fromList kvsDiffVal
    t4 = fromList kvsDiffKey

  annotate "found structural equality"
  (t1 == t2) /== (t1 `structuralEq` t2)

  annotate "found key only equality"
  (t1 == t3) === False

  annotate "found value only equality"
  (t1 == t4) === False
  where
    structuralEq Empty Empty                               = True
    structuralEq (Node l0 (k0,a0) r0) (Node l1 (k1,a1) r1) = l0 == l1 && k0 == k1 && a0 == a1 && r0 == r1
    structuralEq Empty _                                   = False
    structuralEq _ Empty                                   = False
