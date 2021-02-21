module Luhn (isValid) where

import Data.Char
import Data.List
import Debug.Trace

stripSpaces str = Just $ filter (not . isSpace) str

checkChars str =
  str
    >>= ( \str' ->
            if length (filter isDigit str') == length str'
              then str
              else Nothing
        )

checkLenght str = str >>= (\str' -> if length str' > 1 then str else Nothing)

doubleSecondNum :: Maybe String -> Maybe String
doubleSecondNum = fmap (reverse . doubleIt . reverse)
  where
    doubleIt :: String -> String
    doubleIt xs
      | length xs < 2 = xs
      | otherwise =
        let a : b = take 2 xs
            b' = digitToInt (head b)
            b'' = b' * 2
            r = head $ show $ if b'' > 9 then b'' - 9 else b''
         in (a : [r]) ++ doubleIt (drop 2 xs)

checkResult :: Maybe String -> Bool
checkResult Nothing = False
checkResult (Just n) =
  let num = foldr (\x acc -> digitToInt x + acc) 0 n
   in num `mod` 10 == 0

isValid :: String -> Bool
isValid =
  checkResult . doubleSecondNum . checkLenght . checkChars . stripSpaces
