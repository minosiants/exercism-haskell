module Sieve (primesUpTo) where

-- You should not use any of the division operations when implementing
-- the sieve of Eratosthenes.

import qualified Data.List as L
import Prelude hiding ((/), div, divMod, mod, quot, quotRem, rem)

div' :: Integer -> Integer -> (Integer, Integer)
div' a b = go a b 0
  where
    go x y r
      | x < y = (r, x)
      | x == 0 = (r, 0)
      | otherwise = go (x - y) y (r + 1)

mod' :: Integer -> Integer -> Integer
mod' a b = snd $ div' a b

nextP :: Integer -> [(Integer, Bool)] -> Maybe Integer
nextP p xs = fmap fst $ L.find (\(i, s) -> i > p && s == True) xs

mark :: Integer -> (Integer, Bool) -> (Integer, Bool)
mark p (i, s)
  | i == p = (i, s)
  | mod' i p == 0 = (i, False)
  | otherwise = (i, s)

primesUpTo :: Integer -> [Integer]
primesUpTo i
  | i < 2 = []
  | otherwise =
    let nums = fmap (\x -> (x, True)) [2 .. i]
     in go 2 nums
  where
    go :: Integer -> [(Integer, Bool)] -> [Integer]
    go p xs =
      let xs' = fmap (mark p) xs
       in case (nextP p xs') of
            Just p' -> go p' xs'
            Nothing -> fmap fst $ L.filter ((== True) . snd) xs'
