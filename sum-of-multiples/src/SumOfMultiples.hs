module SumOfMultiples (sumOfMultiples) where

import Data.Set

sumOfMultiples :: [Integer] -> Integer -> Integer
sumOfMultiples factors limit =
  sum . toList . fromList $ (factors >>= findMult)
  where
    r = [0 .. limit]
    findMult n =
      fmap (* n) $ takeWhile (\i -> i * n < limit) r
