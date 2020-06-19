module Squares (difference, squareOfSum, sumOfSquares) where

difference :: Integral a => a -> a
difference n = squareOfSum n - sumOfSquares n

squareOfSum :: Integral a => a -> a
squareOfSum n = ssum * ssum
  where
    ssum = sum [0 .. n]

sumOfSquares :: Integral a => a -> a
sumOfSquares n = sum $ squares
  where
    squares = fmap (\i -> i * i) [0 .. n]
