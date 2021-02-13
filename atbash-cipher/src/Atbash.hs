module Atbash (decode, encode) where

import Data.Char (isNumber, isPunctuation, isSpace, toLower)
import Data.List
import Data.Maybe

plain = "abcdefghijklmnopqrstuvwxyz"

cypher = plain `zip` (reverse plain)

encodeChar :: Char -> [Char]
encodeChar ch
  | isNumber ch = [ch]
  | isSpace ch = []
  | isPunctuation ch = []
  | otherwise = go $ toLower ch
  where
    go ch =
      let res = fmap snd $ find (\(a, _) -> a == ch) cypher
       in case res of
            Nothing -> ""
            Just a -> [a]

decodeChar :: Char -> String
decodeChar ch
  | isNumber ch = [ch]
  | otherwise = go ch
  where
    go ch =
      let res = fmap fst $ find (\(_, b) -> b == ch) cypher
       in case res of
            Nothing -> ""
            Just a -> [a]

split :: String -> [String]
split str = go str
  where
    go [] = []
    go str =
      [(take 5 str)] ++ go (drop 5 str)

decode :: String -> String
decode cipherText =
  concat $ fmap decodeChar cipherText

encode :: String -> String
encode ch = unwords . split $ ch >>= encodeChar
