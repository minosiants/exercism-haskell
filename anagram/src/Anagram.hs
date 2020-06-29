module Anagram (anagramsFor) where

import qualified Data.List as L
import qualified Data.Text as T

anagramsFor :: String -> [String] -> [String]
anagramsFor xs xss =
  let word = prepare xs
   in L.filter (notItself xs) $
        L.filter (isAnagram word) xss
  where
    notItself me str = (prepare' me) /= (prepare' str)
    isAnagram me str = (prepare str) == me
    prepare' = T.unpack . T.strip . (T.filter (/= ' ')) . T.toLower . T.pack
    prepare = L.sort . prepare'
