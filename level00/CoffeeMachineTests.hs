{-# LANGUAGE RankNTypes #-}
module CoffeeMachineTests (propertyTests) where

import Control.Lens (Prism', prism, matching, preview, review)

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.Hedgehog (testProperty)

import Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range

-- Starting
addTen :: Int -> Int
addTen = (+10)

prop_addTen :: Property
prop_addTen = property $ do
  n <- forAll $ Gen.int (Range.linear 0 1000)
  addTen n === n + 10

badReverse :: [a] -> [a]
badReverse [] = []
badReverse (_:xs) = reverse xs

prop_badReverse :: Property
prop_badReverse = property $ do
  xs <- forAll (Gen.list (Range.linear 0 1000) Gen.bool)
  badReverse (badReverse xs) === xs
-- End Starting

-- Need a middle ground exercise

-- Recursive structures (??)
data MyTree a
  = Leaf a
  | Node (MyTree a) (MyTree a)
  deriving (Show, Eq)

-- Provide prisms, it's not a lens course
_Node :: Prism' (MyTree a) (MyTree a, MyTree a)
_Node = prism (uncurry Node) (\x -> case x of Node a b -> Right (a,b); _ -> Left x)

_Leaf :: Prism' (MyTree a) a
_Leaf = prism Leaf (\x -> case x of Leaf a -> Right a; _ -> Left x)

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
-- End slightly silly

propertyTests :: TestTree
propertyTests = testGroup "Level00 - Property Tests"
  [ testProperty "Addition still works" prop_addTen
  , testProperty "Bad reverse is bad" prop_badReverse
  , prop_myTreePrisms
  ]
