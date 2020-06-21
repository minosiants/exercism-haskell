module Isogram (isIsogram) where

import qualified Data.List as L
import qualified Data.Text as T

isIsogram :: String -> Bool
isIsogram txt = (L.length stripped) == (L.length $ L.nub stripped)
  where
    stripped :: [Char]
    stripped = T.unpack $ T.toLower . T.strip $ remove ["-", " "] (T.pack txt)
    remove rms _txt = foldr (\rm res -> T.replace (T.pack rm) (T.pack "") res) _txt rms
