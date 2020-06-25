module RunLength (decode, encode) where

import Data.Char
import Data.List

decode :: String -> String
decode [] = []
decode [n] = [n]
decode str =
  if isNumber $ head str
    then
      let (num, rest) = span isNumber str
       in chars (read num) (head rest) ++ decode (tail rest)
    else [head str] ++ decode (tail str)
  where
    chars :: Int -> Char -> String
    chars i ch = take i $ repeat ch

encode :: String -> String
encode text =
  (group text)
    >>= \x ->
      let lx = (length x)
          num = if lx == 1 then "" else show lx
       in num ++ [head x]
