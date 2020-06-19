module Acronym (abbreviate) where

import Data.Char
import Data.List

replace :: Char -> Char -> String -> String
replace from to str = replaceAll (findIndices (== from) str) str
  where
    replaceAll :: [Int] -> String -> String
    replaceAll [] s = s
    replaceAll (i : is) s =
      case (splitAt i s) of
        (l, (_ : xs)) -> replaceAll is (l ++ [to] ++ xs)
        (_, _) -> s

replaceDashes :: String -> String
replaceDashes = replace '-' ' '

replaceUnderlines :: String -> String
replaceUnderlines = replace '_' ' '

splitOnCap :: String -> [String]
splitOnCap str
  | all isUpper str = [str]
  | otherwise = go str [] []
  where
    go :: [Char] -> String -> [String] -> [String]
    go s ss acc = case s of
      [] -> acc ++ [ss]
      x : xs ->
        if (isUpper x) && null ss
          then go xs [x] (acc)
          else
            if isUpper x
              then go xs [x] (acc ++ [ss])
              else go xs (ss ++ [x]) acc

split :: String -> [String]
split xs = (words $ (replaceDashes . replaceUnderlines) xs) >>= splitOnCap

abbreviate :: String -> String
abbreviate xs = fmap (toUpper . head) $ split xs
