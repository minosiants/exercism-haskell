module PrimeFactors (primeFactors) where

primeFactors :: Integer -> [Integer]
primeFactors n = go n 2
  where
    go x divisor
      | x == 1 = []
      | x `mod` divisor == 0 = divisor : go (x `div` divisor) divisor
      | otherwise = go x (divisor + 1) 
