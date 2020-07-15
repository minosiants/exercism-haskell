module Triplet (tripletsWithSum) where

sqrt' :: Int -> Int
sqrt' = floor . sqrt . fromIntegral

tripletsWithSum :: Int -> [(Int, Int, Int)]
tripletsWithSum sum' =
  filter
    ( \(a, b, c) ->
        a * a + b * b == c * c
          && a + b + c == sum'
    )
    [(a, b, sqrt' (a * a + b * b)) 
      | a <- [1 .. sum'], b <- [1 .. sum'], a < b]
