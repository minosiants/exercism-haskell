module Say (inEnglish) where
import Debug.Trace
import Data.List
import Control.Applicative ( Alternative((<|>)) )

inEnglish :: Integer -> Maybe String
inEnglish n
  | n < 0 = Nothing
  | n > 999999999999 = Nothing
  | otherwise =  fmap toString $ traverse convert $ hundredPart $ numberParts n

convert :: Integer -> Maybe String
convert n =
  let p1 = n `div` 10 * 10
      p2 = n `mod` 10
   in go p1 p2
  where
    h = (: []) <$> number n
    go 0 p2 = number p2
    go p1 p2 =
          intercalate "-" <$> (h <|> sequence [number p1, number p2])


toString :: [String] -> String
toString xs
  | null p1 = p2
  | p2 == "zero" = p1
  |otherwise = p1 ++ " " ++ p2
  where
    p1 = parts $ reverse $ drop 1 $ reverse xs
    p2 = last xs
    parts [] = ""
    parts (x:xs') =
         if x /= "zero" 
         then x ++ inLetters xs' ++ parts xs'
         else parts xs'
    inLetters xs'  =
      case length xs' of
         0 ->  " hundred"
         1 ->  " thousand"
         2 -> " million"
         3 -> " billion"
         _ -> error ""


hundredPart :: [Integer] -> [Integer]
hundredPart  xs
    | h == 0 =  reverse (drop 1  $ reverse xs) ++ [0,rest]
    | otherwise = reverse (drop 1  $ reverse xs) ++ [h, rest]
    where
      n = last xs
      h = n `div` 100
      rest = n `mod` 100


numberParts :: Integer -> [Integer]
numberParts x
     | num == 0 = [rest]
     | otherwise = numberParts num ++ [rest]
   where
     num = x `div` 1000
     rest = x `mod` 1000

number :: Integer  -> Maybe String
number n = case n of
  0 -> Just "zero"
  1 -> Just "one"
  2 -> Just "two"
  3 -> Just "three"
  4 -> Just "four"
  5 -> Just "five"
  6 -> Just "six"
  7 -> Just "seven"
  8 -> Just "eight"
  9 -> Just "nine"
  10 -> Just "ten"
  11 -> Just "eleven"
  12 -> Just "twelve"
  13 -> Just "thirteen"
  14 -> Just "fourteen"
  15 -> Just "fifteen"
  16 -> Just "sixteen"
  17 -> Just "seventeen"
  18 -> Just "eighteen"
  19 -> Just "nineteen"
  20 -> Just "twenty"
  30 -> Just "thirty"
  40 -> Just "forty"
  50 -> Just "fifty"
  60 -> Just "sixty"
  70 -> Just "seventy"
  80 -> Just "eighty"
  90 -> Just "ninety"
  _  -> Nothing
