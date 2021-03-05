module CustomSet
  ( delete,
    difference,
    empty,
    fromList,
    insert,
    intersection,
    isDisjointFrom,
    isSubsetOf,
    member,
    null,
    size,
    toList,
    union,
  )
where

import qualified Data.List as L
import Prelude hiding (null)

newtype CustomSet a = CustomSet [a]

instance (Eq a) => Eq (CustomSet a) where
  (==) xs ys
    | null xs && null ys = True
    | otherwise = 0 == size (difference xs ys) && 0 == size (difference ys xs)

instance (Show a) => Show (CustomSet a) where
  show (CustomSet xs) = show xs

delete :: Eq a => a -> CustomSet a -> CustomSet a
delete x (CustomSet xs) = CustomSet $ L.filter (/= x) xs

difference :: Eq a => CustomSet a -> CustomSet a -> CustomSet a
difference (CustomSet xs) (CustomSet ys) =
  CustomSet $ xs L.\\ ys

empty :: CustomSet a
empty = CustomSet []

fromList :: Eq a => [a] -> CustomSet a
fromList = foldr insert empty

insert :: Eq a => a -> CustomSet a -> CustomSet a
insert x cs@(CustomSet xs) =
  case L.find (== x) xs of
    Nothing -> CustomSet (x : xs)
    Just _ -> cs

intersection :: Eq a => CustomSet a -> CustomSet a -> CustomSet a
intersection (CustomSet xs) setB =
  CustomSet $ foldr (\x acc -> if member x setB then x : acc else acc) [] xs

isDisjointFrom :: Eq a => CustomSet a -> CustomSet a -> Bool
isDisjointFrom xs ys =
  case (null xs, null ys) of
    (False, False) -> null $ intersection xs ys
    (_, _) -> True

isSubsetOf :: Eq a => CustomSet a -> CustomSet a -> Bool
isSubsetOf (CustomSet xs) setB = all (== True) $ fmap (`member` setB) xs

member :: Eq a => a -> CustomSet a -> Bool
member x (CustomSet xs) = L.elem x xs

null :: CustomSet a -> Bool
null (CustomSet xs) = L.null xs

size :: CustomSet a -> Int
size (CustomSet xs) = L.length xs

toList :: CustomSet a -> [a]
toList (CustomSet xs) = xs

union :: Eq a => CustomSet a -> CustomSet a -> CustomSet a
union (CustomSet xs) setb = foldr insert setb xs
