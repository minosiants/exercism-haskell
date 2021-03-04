module Spiral (spiral) where

import Data.List (transpose)

spiral :: Int -> [[Int]]
spiral size = matrixSpiral size size 1

-- https://www.reddit.com/r/dailyprogrammer/comments/6i60lr/20170619_challenge_320_easy_spiral_ascension/dj4krsq?utm_source=share&utm_medium=web2x&context=3
matrixSpiral :: Int -> Int -> Int -> [[Int]]
matrixSpiral i j s
  | i == 0 = []
  | otherwise = [s .. s + j -1] : (map reverse . transpose) (matrixSpiral j (i -1) (s + j))
