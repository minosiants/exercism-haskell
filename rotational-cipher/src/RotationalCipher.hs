module RotationalCipher (rotate) where

import Data.Char
import Data.List

rotate :: Int -> String -> String
rotate rot =
  foldr (\l acc -> encode l rot : acc) ""

letters = "abcdefghijklmnopqrstuvwxyz"

encode :: Char -> Int -> Char
encode ch rot =
  maybe ch applyCypher (elemIndex letter letters)
  where
    letter = toLower ch
    applyCypher i =
      let ci = (i + rot) `mod` 26
          res = head $ drop ci letters
       in if isUpper ch then toUpper res else res
