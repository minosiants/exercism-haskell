module BST
  ( BST,
    bstLeft,
    bstRight,
    bstValue,
    empty,
    fromList,
    insert,
    singleton,
    toList,
  )
where

import Data.List (foldl')

data BST a
  = Leaf
  | Node (BST a) a (BST a)
  deriving (Eq, Show)

bstLeft :: BST a -> Maybe (BST a)
bstLeft Leaf = Nothing
bstLeft (Node l _ _) = Just l

bstRight :: BST a -> Maybe (BST a)
bstRight Leaf = Nothing
bstRight (Node _ _ r) = Just r

bstValue :: BST a -> Maybe a
bstValue Leaf = Nothing
bstValue (Node _ x _) = Just x

empty :: BST a
empty = Leaf

fromList :: Ord a => [a] -> BST a
-- fromList = foldr insert empty
fromList = foldl' (flip insert) empty

insert :: Ord a => a -> BST a -> BST a
insert x Leaf = singleton x
insert x (Node left v right)
  | x == v = Node (singleton x) v right
  | x > v = Node left v (insert x right)
  | x < v = Node (insert x left) v right

singleton :: a -> BST a
singleton x = Node empty x empty

toList :: BST a -> [a]
toList bst =
  case bst of
    Leaf -> []
    (Node Leaf v right) -> v : toList right
    (Node left v Leaf) -> toList left ++ [v]
    (Node left v right) -> toList left ++ [v] ++ toList right
