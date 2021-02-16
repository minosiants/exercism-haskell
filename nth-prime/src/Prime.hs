module Prime (nth) where

import Data.List
import Data.Maybe
import Debug.Trace

-- My version is very ugly

-- Generate infinit sequense of natural numbers
-- Filter out primes
-- https://exercism.io/tracks/haskell/exercises/nth-prime/solutions/1f4e8f7f0b0146af957aef143aa50dbb

mkPrimes :: Integer -> [(Integer, Bool)] -> [(Integer, Bool)]
mkPrimes p xs' =
  case (find (\(i, prime) -> prime == True && i >= p)) xs' of
    Nothing -> xs'
    Just ip ->
      foldr (markNotPrime ip) [] xs'
      where
        markNotPrime (p, _) (i, s) acc
          | i `mod` p == 0 && p /= i = (i, False) : acc
          | otherwise = (i, s) : acc

nth :: Int -> Maybe Integer
nth 1 = Just 2
nth n =
  findNth n $
    mkPrimes
      2
      [(i, True) | i <- [2 .. n' * n']]
  where
    n' = toInteger n
    findNth :: Int -> [(Integer, Bool)] -> Maybe Integer
    findNth n xs =
      let res = fmap fst $ take (n + 1) $ filter (\(_, s) -> s == True) xs
       in case res of
            [] -> Nothing
            l -> Just $ last l
