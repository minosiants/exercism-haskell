module ArmstrongNumbers (armstrong) where

toDigits :: Integral a => a -> [a]
toDigits 0 = []
toDigits n = toDigits (n `div` 10) ++ [n `mod` 10]

armstrong :: Integral a => a -> Bool
armstrong n = (sum $ fmap (^ pow) digits) == n
  where
    digits = toDigits n
    pow = length digits
