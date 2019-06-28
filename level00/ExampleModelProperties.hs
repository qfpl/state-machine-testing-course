module ExampleModelProperties where

import Hedgehog (MonadGen, PropertyT, Property, property, forAll, (===), withTests)
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map

import MyBTree

----------------------------------------------------------------------------
-- This module contains an example of an alternative technique for        --
-- testing structures and simple actions. There are no exercises here. :) --
----------------------------------------------------------------------------

-- Sometimes when testing a given thing, you may be able to use
-- existing data structures or functions as a reference for your
-- implementation. In this case it may be useful to create a mini-DSL
-- that you can evaluate using the thing under test, or using the
-- reference implementation. Comparing the results of the evaluations
-- will be the test.

-- We will use the insert and delete actions for MyBTree to demonstrate this.

-- Use a data structure to represent the actions.
data Model k v
  = Insert k v (Model k v)      -- Inserting a value 'v' at key 'k'
  | Delete k (Model k v)        -- Deleting the key and value at 'k'
  | EmptyModel                  -- No action
  deriving (Show, Eq)

-- We need to evalute our DSL using 'MyBTree'
eval :: Ord k => Model k v -> MyBTree k v
eval (Insert k v m) = insert k v (eval m)
eval (Delete k m) = deleteKey k (eval m)
eval EmptyModel = Empty

-- We also need to be able to evaluate the actions against a
-- reference. For this we will use the 'Set' type from the
-- 'containers' package.
model :: Ord k => Model k v -> Map k v
model (Insert k v m) = Map.insert k v (model m)
model (Delete k m) = Map.delete k (model m)
model EmptyModel = Map.empty

-- We can then write a general property that can accept a 'Model',
-- evaluates the DSL against our thing being tested as well as the
-- reference and ensures that they should produce the same output,
-- when reduced to a suitable comparison type. In this case, an
-- ascending list.
prop_mybtree :: (Monad m, Show k, Show v, Eq v, Ord k) => Model k v -> PropertyT m ()
prop_mybtree m = toListWithKey (eval m) === Map.toAscList (model m)

-- This DSL now gives us the flexiblity to test our structure against
-- various implementations in different situations without having to
-- necessarily duplicate a lot of code. Additionally it is trivial to
-- write 'unit tests' that use this DSL, but we've also now created
-- the possibility of writing a generator for our DSL. Thus we can
-- lean on Hedgehog to generate sequences of actions.

-- Some unit tests
prop_single_insert :: Property
prop_single_insert = withTests 1 . property $
  prop_mybtree (Insert 1 'a' EmptyModel)

prop_single_delete :: Property
prop_single_delete = withTests 1 . property $
  prop_mybtree (Delete 1 (Insert 1 'a' EmptyModel))

-- These unit tests leave something to be desired but hopefully they
-- serve as a sufficient demonstration of the point. Namely, they lack
-- coverage.

-- About that generator...
--
-- Our model is a recursive structure, so we leverage the functions in
-- Hedgehog to manage that recursion for us. Lest we have an infinite
-- model. This also allows Hedgehog greater control of shrinking which
-- provides greater accuracy for us.
genModel :: (Show k, Show v, Eq v, Ord k, MonadGen m) => m k -> m v -> m (Model k v)
genModel genK genV = Gen.recursive Gen.choice
  -- Hedgehog shrinks towards the non-recursive base case(s)
  [ pure EmptyModel
  ]
  -- Create the subterms for our Model that separate the recursive component into a separate function.
  [ Gen.subtermM (genModel genK genV) (\m -> Insert <$> genK <*> genV <*> pure m)
  , Gen.subtermM (genModel genK genV) (\m -> Delete <$> genK <*> pure m)
  ]

-- Now we can write a more thorough test of our structure using the
-- generator that will provide far greater coverage.
prop_mybtree_gen_model :: Property
prop_mybtree_gen_model = property $
  let
    genKey = Gen.int $ Range.linear (-100) 100
    genVal = Gen.text (Range.linear 0 100) Gen.ascii
  in
    forAll (genModel genKey genVal) >>= prop_mybtree
