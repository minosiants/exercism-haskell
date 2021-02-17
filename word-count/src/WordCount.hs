module WordCount (wordCount) where

import Data.Char
import Data.List
import qualified Data.MultiSet as M
import Debug.Trace

isApostrophe = (== '\'')

replaceWithSpace :: Char -> Char -> Char
replaceWithSpace del ch
  | ch == del = ' '
  | otherwise = ch

removeQuotes :: String -> String
removeQuotes txt = unwords $ (fmap) go $ words txt
  where
    go xs
      | (quotes xs) > 1 = fmap (replaceWithSpace '\'') xs
      | otherwise = xs
    quotes = foldr (\ch acc -> if ch == '\'' then acc + 1 else acc) 0

clean :: String -> String
clean xs =
  filter
    ( \ch ->
        (isAlpha ch)
          || (isAlphaNum ch)
          || (isApostrophe ch)
          || (isSpace ch)
    )
    $ fmap (toLower . replaceWithSpace ',')
    $ xs

wordCount :: String -> [(String, Int)]
wordCount = (M.toOccurList) . (M.fromList) . words . clean . removeQuotes
