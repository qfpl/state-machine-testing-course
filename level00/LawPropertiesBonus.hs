module LawPropertiesBonus where

-- Recursive structures
data MyTree a
  = Leaf a
  | Node (MyTree a) (MyTree a)
  deriving (Show, Eq)

-- Fun generator to write, requires documentation diving (too much?)
genMyTree :: Gen a -> Gen (MyTree a)
genMyTree = error "genMyTree not implemented"
-- genMyTree g = Gen.recursive Gen.choice 
--   [ Leaf <$> g ]
--   [ Node <$> genMyTree g <*> genMyTree g ]

-- First, if I re or review a value with a Prism and then preview or use (^?),
-- I will get it back:
-- 
-- preview l (review l b) ≡ Just b
-- 
firstPrismLaw :: (Eq b, Show b) => Gen b -> Prism' a b -> Property
firstPrismLaw = error "firstPrismLaw not implemented"
-- firstPrismLaw genB l = property $ forAll genB >>= \b ->
--   preview l (review l b) === Just b

-- Second, if you can extract a value a using a Prism l from a value s, then
-- the value s is completely described by l and a:
-- 
-- preview l s ≡ Just a ==> review l a ≡ s
secondPrismLaw :: (Eq s, Show s) => Gen s -> Prism' s a -> Property
secondPrismLaw = error "secondPrismLaw not implemented"
-- secondPrismLaw genS l = property $ forAll genS >>= \s ->
--   case preview l s of
--     Just a -> review l a === s
--     _ -> success -- Not a matching value, disregard

-- Third, if you get non-match t, you can convert it result back to s:
-- 
-- matching l s ≡ Left t ==> matching l t ≡ Left s
thirdPrismLaw :: (Eq a, Show a, Eq s, Show s) => Gen s -> Prism' s a -> Property
thirdPrismLaw = error "thirdPrismLaw not implemented"
-- thirdPrismLaw genS l = property $ forAll genS >>= \s ->
--   case matching l s of
--     Left t -> matching l t === Left s
--     Right _ -> success  -- Not a matching value, disregard

