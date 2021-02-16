# Exercism - Track Haskell

Solutions for haskell track excersies at [exercism.io](https://exercism.io)

## nice solutions 

### [Nth Prime](https://exercism.io/tracks/haskell/exercises/nth-prime/solutions/1f4e8f7f0b0146af957aef143aa50dbb )
```haskell
module Prime (nth) where

nth :: Int -> Maybe Integer
nth n
  | n > 0 = Just $ primes [2..]!!(n - 1)
  | otherwise = Nothing

primes :: [Integer] -> [Integer]
primes (x:xs) = x:primes (filter (\y -> y `mod` x /= 0) xs)
primes [] = []

```
What I forgot here:
  - lazy infinit collections.
  `[2..]` generate infinit secuance. then filter out primes only   

For more info https://wiki.haskell.org/Prime_numbers

