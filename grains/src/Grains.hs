module Grains (square, total) where

import Data.Bits
import Data.Maybe (fromMaybe)

square :: Integer -> Maybe Integer
square n
  | n <= 0 = Nothing
  | n > 64 = Nothing
  | otherwise = Just $ 1 `shift` shiftVal
  where
    shiftVal = (fromIntegral n) -1

total :: Integer
total = fromMaybe 0 (square 64) * 2 -1
