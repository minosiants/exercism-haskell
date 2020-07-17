module Series (slices) where

import Data.Char (digitToInt)

slices :: Int -> String -> [[Int]]
slices n xs
  | length xs < n = []
  | length xs == n = [fmap digitToInt xs]
  | otherwise =
    let part = fmap digitToInt $ take n xs
     in part : slices n (tail xs)
