module IsbnVerifier (isbn) where

import Data.Char (isNumber, toUpper)
import Data.Maybe (fromMaybe)

parse :: String -> Maybe [Int]
parse str =
  case reverse str of
    ('X' : xs) -> traverse parseNum (reverse xs) <> Just [10]
    _ -> traverse parseNum str
  where
    parseNum :: Char -> Maybe Int
    parseNum ch
      | isNumber ch = Just $ read [ch]
      | otherwise = Nothing

filterNumbers :: String -> String
filterNumbers = filter (\ch -> isNumber ch || ch == 'X')

validate :: [Int] -> Bool
validate nums
  | length nums /= 10 = False
  | otherwise =
    ( sum
        $ fmap (uncurry (*))
        $ zip nums [10, 9 .. 1]
    )
      `mod` 11
      == 0

isbn :: String -> Bool
isbn num = fromMaybe False $ do
  digits <- (parse . filterNumbers . fmap toUpper) num
  return (validate digits)
