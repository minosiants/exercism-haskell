module Pangram (isPangram) where

import Data.Char (isDigit, toLower)

removeLetters :: Char -> String -> String
removeLetters l = filter (/= l)

hasLetter :: Char -> String -> Bool
hasLetter = elem

toLoverCase :: String -> String
toLoverCase = fmap toLower

removeNumbers :: [String] -> [String]
removeNumbers = filter isNotNumber
  where
    isNotNumber str = not $ all isDigit str

removeDashes :: String -> String
removeDashes = filter (/= '_')

removeDots :: String -> String
removeDots = filter (/= '.')

removeQuotes :: String -> String
removeQuotes = filter (/= '"')

removePunctuation :: String -> String
removePunctuation = removeDashes . removeDots . removeQuotes

isPangram :: String -> Bool
isPangram text = process ['a' .. 'z'] $ (toLoverCase . removePunctuation . concat . removeNumbers . words) text
  where
    process letters txt =
      case (letters, txt) of
        (_ : _, []) -> False
        ([], _) -> True
        (x : xs, t) ->
          if hasLetter x t
            then process xs (removeLetters x t)
            else False
