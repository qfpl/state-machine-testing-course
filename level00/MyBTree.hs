{-# LANGUAGE DeriveFoldable    #-}
{-# LANGUAGE DeriveFunctor     #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE DeriveGeneric     #-}
module MyBTree
  ( MyBTree (..)
  , insert
  , deleteKey
  , fromList
  , toListWithKey
  ) where

import GHC.Generics
import Data.Foldable (foldl')
import Hedgehog.Function

-- Data structure and functions inspired by a presentation by John Hughes: "Building on developer intuitions". 
-- Which may be viewed at: https://www.youtube.com/watch?v=NcJOiQlzlXQ
--

data MyBTree k a
  = Empty
  | Node (MyBTree k a) (k,a) (MyBTree k a)
  deriving (Show, Functor, Traversable, Foldable, Generic)

instance (Arg k, Arg v) => Arg (MyBTree k v)
instance (Vary k, Vary v) => Vary (MyBTree k v)

fromList :: (Foldable f, Ord k) => f (k,a) -> MyBTree k a
fromList = foldl' (\m (k, a) -> insert k a m) Empty

toListWithKey :: MyBTree k a -> [(k,a)]
toListWithKey Empty         = []
toListWithKey (Node l kv r) = toListWithKey l <> [kv] <> toListWithKey r

insert :: Ord k => k -> a -> MyBTree k a -> MyBTree k a
insert k v t = case t of
  Empty -> Node Empty (k, v) Empty
  Node l (k0,v0) r -> case compare k k0 of
    LT -> Node (insert k v l) (k0, v0) r
    EQ -> Node l (k,v) r
    GT -> Node l (k0, v0) (insert k v r)

deleteKey :: (Eq k, Ord k) => k -> MyBTree k a -> MyBTree k a
deleteKey _ Empty                                 = Empty
deleteKey k (Node l (k0,v0) r) = case compare k k0 of
  LT -> Node (deleteKey k l) (k0,v0) r
  GT -> Node l (k0,v0) (deleteKey k r)
  EQ -> case (l,r) of
    (Empty, Empty) -> Empty
    (Empty, r0)    -> r0
    (l0   , Empty) -> l0
    _              ->
      let
        minkv = findMin r
        newR  = deleteKey (fst minkv) r
      in
        Node l minkv newR
  where
    findMin (Node Empty kv _) = kv
    findMin (Node leftT _ _)  = findMin leftT
    findMin _                 = error "impossible?"
