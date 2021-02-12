module BinarySearch (find) where

import Data.Array
import Debug.Trace

find :: (Show a, Ord a) => Array Int a -> a -> Maybe Int
find array x =
  let l = ((length . assocs) array)
   in find' 0 l
  where
    find' from till =
      let half = (from + till) `div` 2
          x' = array ! half
       in doIt half x'
      where
        doIt half x'
          | till == 0 = Nothing
          | x == x' = Just $ half
          | (from + till) == 0 = Nothing
          | x < x' = find' from half
          | from == half = Nothing
          | otherwise = find' half till
