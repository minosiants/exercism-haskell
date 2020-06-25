module Triangle (TriangleType (..), triangleType) where

import Data.List

data TriangleType
  = Equilateral
  | Isosceles
  | Scalene
  | Illegal
  deriving (Eq, Show)

triangleType :: (Num a, Ord a) => a -> a -> a -> TriangleType
triangleType a b c
  | a <= 0 || b <= 0 || c <= 0 = Illegal
  | (sum . take 2) sides < last sides = Illegal
  | (length . nub) sides == 1 = Equilateral 
  | (length . nub) sides == 3 = Scalene
  | (length . group) sides >=2 = Isosceles
  | otherwise = Illegal
  where
    sides = sort [a, b, c]
