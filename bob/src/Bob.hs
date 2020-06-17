module Bob (responseFor) where

import Data.Char
import Data.Monoid

strip :: String -> String
strip s =
  reverse
    $ dropWhile (== ' ')
    $ reverse
    $ dropWhile (== ' ') s

yelling :: String -> Bool
yelling s
  | xs == [] = False
  | otherwise = getAll (foldMap (All . isUpper) xs)
  where
    xs = filter isLetter s

question :: String -> Bool
question [] = False
question s = last s == '?'

yellingQuestion :: String -> Bool
yellingQuestion s = yelling s && question s

silence :: String -> Bool
silence =
  null
    . filter
      (\x -> not (isControl x) && not (x == ' '))

responseFor :: String -> String
responseFor s
  | silence xs = "Fine. Be that way!"
  | yellingQuestion xs = "Calm down, I know what I'm doing!"
  | yelling xs = "Whoa, chill out!"
  | question xs = "Sure."
  | otherwise = "Whatever."
  where 
    xs = strip s
