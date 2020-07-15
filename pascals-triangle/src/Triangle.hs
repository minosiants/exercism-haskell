module Triangle (rows) where

import Data.List (find)
import Data.Maybe (fromMaybe)

zipWithIndex :: [a] -> [(Int, a)]
zipWithIndex xs =
  zipWith (,) [0 .. (length xs)] xs

find' :: Int -> [(Int, a)] -> Maybe a
find' i xs = fmap snd $ find ((== i) . fst) xs

element :: Int -> [Integer] -> Integer
element depth lastRow
  | (length lastRow) < depth = 1
  | otherwise =
    let indexed = zipWithIndex lastRow
     in fromMaybe
          1
          ( do
              a <- find' (depth -2) indexed
              b <- find' (depth -1) indexed
              return $ a + b
          )

rows' :: Int -> [[Integer]]
rows' x = go 1 []
  where
    go depth res
      | depth > x = res
      | depth == 1 = go (depth + 1) (res ++ [[1]])
      | otherwise = go (depth + 1) (res ++ [row depth (last res)])
    row :: Int -> [Integer] -> [Integer]
    row depth lastRow
      | depth == 1 = [1]
      | otherwise = (element depth lastRow) : (row (depth -1) lastRow)

-- The simple one :)

rows :: Int -> [[Integer]]
rows n = take n $ iterate f [1]
  where
    f xs = zipWith (+) (0 : xs) (xs ++ [0])
