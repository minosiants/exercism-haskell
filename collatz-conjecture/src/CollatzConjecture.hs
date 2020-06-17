module CollatzConjecture (collatz) where

collatz' :: Integer -> Maybe Integer
collatz' num
  | num <= 0 = Nothing
  | otherwise = go num 0
    where
      go n count =
        if n == 1
          then Just count
          else
            if n `mod` 2 == 0
              then go (n `div` 2) (count + 1)
              else go (n * 3 + 1) (count + 1)

collatz :: Integer -> Maybe Integer
collatz num 
  | num < 1 = Nothing
  | otherwise = go num 0
    where
      go n count 
        | n == 1 = Just count
        | even n = go (n `div` 2) (count +1)
        | otherwise = go (n *3 +1) (count +1)
