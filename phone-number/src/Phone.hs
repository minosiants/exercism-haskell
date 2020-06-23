module Phone (number) where

import Data.Char (isNumber)

number :: String -> Maybe String
number = validate . filter isNumber
  where
    validate :: String -> Maybe String
    validate num
      | length num < 10 = Nothing
      | reverse num !! 6 < '2' || reverse num !! 9 < '2' = Nothing
      | head num == '1' && (length . tail) num == 10 = Just $ tail num
      | length num == 10 = Just num
      | otherwise = Nothing
