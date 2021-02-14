module CryptoSquare (encode) where

import Data.Char (isNumber, isPunctuation, isSpace, toLower)

normolize :: String -> String
normolize str = str >>= go
  where
    go ch
      | isNumber ch = [ch]
      | isSpace ch = []
      | isPunctuation ch = []
      | otherwise = [toLower ch]

sqrt' :: Int -> Int
sqrt' = floor . sqrt . fromIntegral

recSize :: String -> (Int, Int)
recSize txt =
  let chars = length txt
      rows = sqrt' chars
      chunks = if (rows * rows < chars) then rows + 1 else rows
   in (chunks, rows)

pad :: Int -> Char -> String -> String
pad c ch str =
  str ++ replicate (c - length str) ch

mkRectangle :: Int -> Int -> String -> [String]
mkRectangle _ _ [] = []
mkRectangle chunks rows xs =
  let padded = pad chunks ' ' $ take chunks xs
   in [padded] ++ (mkRectangle chunks rows (drop chunks xs))

cipher :: Int -> [String] -> [String]
cipher column xs =
  let row = foldr (\x acc -> (x !! column) : acc) [] xs
   in if column == 0 then [row] else (cipher (column -1) xs) ++ [row]

-- try to use transpose https://hackage.haskell.org/package/base-4.14.1.0/docs/Data-List.html#v:transpose
-- instead of cipher
encode :: String -> String
encode [] = ""
encode txt =
  unwords $ cipher (chunks - 1) rectangle1
  where
    normolized = normolize txt
    (chunks, rows) = recSize normolized
    rectangle1 = mkRectangle chunks rows normolized
