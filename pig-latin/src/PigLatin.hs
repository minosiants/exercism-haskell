module PigLatin (translate) where

import Debug.Trace 

translate :: String -> String
translate xs = unwords $ fmap go (words xs) 
 where 
   go xs
      | vowelFirst xs = xs ++ "ay"
      | twoLetters xs = makeVowel xs
      | consonantFirst xs && hasY xs = toEndY xs
      | consonantFirst xs = toEnd xs
      |  otherwise = xs



vowelFirst :: String -> Bool
vowelFirst "" = False
vowelFirst (x : xs')
      | x `elem` "aeiou" = True
      | not (null xs') && x == 'x' && head xs' == 'r' = True
      | not (null xs') && x == 'y' && head xs' == 't' = True
      | otherwise = False

makeVowel :: String -> String
makeVowel "" = error ""
makeVowel (x:_) = "y"++ [x] ++"ay"

consonants :: String
consonants = "bcdfghjklmnpqrstvwxyz"

twoLetters :: String -> Bool
twoLetters "" = False
twoLetters xs = last xs =='y' && length xs == 2

hasY :: String -> Bool
hasY xs =
   not (null pref)&& length rest > 1 && head rest == 'y'
  where
    (pref, rest) = splitY xs


firstConsonants :: String -> String
firstConsonants = takeWhile (`elem` consonants)

splitY :: String -> (String, String)
splitY xs = 
  splitAt (length pref) xs
  where  
     pref = takeWhile (/= 'y') xs 

consonantFirst :: String -> Bool
consonantFirst xs'
      | firstConsonants xs' /= "" = True
      | otherwise = False


splitConso :: String -> (String, String)
splitConso xs =  splitAt  (length  $ firstConsonants xs) xs

toEndY :: String -> String
toEndY xs=
   let res = rest ++ conso ++ "ay"
   in trace ("rest: "++ show rest ++ " conso: " ++ conso) res
  where
  (conso, rest) = splitY xs


toEnd :: String -> String
toEnd xs
        | not (null rest) && last conso == 'q' && head rest== 'u' = tail rest ++ conso ++ ['u'] ++ "ay"
        | otherwise = rest ++ conso ++ "ay"
      where
        (conso, rest) = splitConso xs
