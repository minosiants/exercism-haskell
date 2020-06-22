module Raindrops (convert) where

pling :: Int -> String
pling n
  | n `mod` 3 == 0 = "Pling"
  | otherwise = ""

plang :: Int -> String
plang n
  | n `mod` 5 == 0 = "Plang"
  | otherwise = ""

plong :: Int -> String
plong n
  | n `mod` 7 == 0 = "Plong"
  | otherwise = ""

convert :: Int -> String
convert n =
  case foldr
    (\f acc -> (f n) ++ acc)
    ""
    [pling, plang, plong] of
    "" -> show n
    v -> v
