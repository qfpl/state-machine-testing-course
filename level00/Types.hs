
data MyTree a
  = Leaf a
  | Node (MyTree a) (MyTree a)
  deriving (Show, Eq)

_Node :: Prism' (MyTree a) (MyTree a, MyTree a)
_Node = prism (uncurry Node) (\x -> case x of Node a b -> Right (a,b); _ -> Left x)

_Leaf :: Prism' (MyTree a) a
_Leaf = prism Leaf (\x -> case x of Leaf a -> Right a; _ -> Left x)