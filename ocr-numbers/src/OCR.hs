{-# LANGUAGE LambdaCase #-}

module OCR
  ( convert,
  )
where

import Data.List

parce :: String -> String
parce xs
  | length xs < 4 = error "size is not correct"
  | otherwise =
    case lines xs of
      [" _ ", "| |", "|_|", "   "] -> "0"
      ["   ", "  |", "  |", "   "] -> "1"
      [" _ ", " _|", "|_ ", "   "] -> "2"
      [" _ ", " _|", " _|", "   "] -> "3"
      ["   ", "|_|", "  |", "   "] -> "4"
      [" _ ", "|_ ", " _|", "   "] -> "5"
      [" _ ", "|_ ", "|_|", "   "] -> "6"
      [" _ ", "  |", "  |", "   "] -> "7"
      [" _ ", "|_|", "|_|", "   "] -> "8"
      [" _ ", "|_|", " _|", "   "] -> "9"
      _ -> "?"

convert :: String -> String
convert txt =
  let rows = splitRows txt
      digits r =
        concatMap parce $ splitDigits r
      res = fmap digits rows
   in intercalate "," res

splitDigits :: String -> [String]
splitDigits txt =
  zipWith4 toDigit (lines' !! 0) (lines' !! 1) (lines' !! 2) (lines' !! 3)
  where
    lines' = chunkes 3 <$> lines txt
    toDigit a b c d = unlines [a, b, c, d]

splitRows :: String -> [String]
splitRows txt =
  let l = lines txt
      ch = chunkes 4 l
      r = (fmap . fmap) (++ "\n") ch
   in if length l == 4
        then [txt]
        else fmap concat r

chunkes step =
  unfoldr
    ( \case
        [] -> Nothing
        str -> Just $ splitAt step str
    )
