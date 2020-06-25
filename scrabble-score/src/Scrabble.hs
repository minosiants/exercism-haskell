module Scrabble (scoreLetter, scoreWord) where

import Data.Char
import Data.List
import Data.Maybe

type Score = Integer

scores :: [(Score, String)]
scores =
  [ (1, "AEIOULNRST"),
    (2, "DG"),
    (3, "BCMP"),
    (4, "FHVWY"),
    (5, "K"),
    (8, "JX"),
    (10, "QZ")
  ]

scoreLetter :: Char -> Integer
scoreLetter letter =
  fromMaybe 0
    $ fmap fst
    $ find (\x -> (length . filter (== toUpper letter) $ (snd x)) /= 0) scores

scoreWord :: String -> Integer
scoreWord = sum . fmap scoreLetter
