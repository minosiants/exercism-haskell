module House (rhyme) where

import Data.Maybe (fromMaybe)
import Data.Text (pack, stripPrefix, unpack)

participants =
  [ ("house that Jack built.", ""),
    ("malt", "lay in"),
    ("rat", "ate"),
    ("cat", "killed"),
    ("dog", "worried"),
    ("cow with the crumpled horn", "tossed"),
    ("maiden all forlorn", "milked"),
    ("man all tattered and torn", "kissed"),
    ("priest all shaven and shorn", "married"),
    ("rooster that crowed in the morn", "woke"),
    ("farmer sowing his corn", "kept"),
    ("horse and the hound and the horn", "belonged to")
  ]

type Participant = (String, String)

replacePrefix :: String -> String -> String -> String
replacePrefix pref rep txt = rep ++ (fromMaybe "" $ fmap unpack (stripPrefix (pack pref) (pack txt)))

create :: [Participant] -> [[String]] -> String
create [] result = (concat . concat) result
create ((p, "") : xs) _ =
  let s = "This is the " ++ p ++ "\n\n"
   in create xs [[s]]
create ((p, action) : xs) result =
  let s = "This is the " ++ p ++ "\n"
      passage' = last result
      sentence = head passage'
      rest = replacePrefix "This is" ("that " ++ action) sentence
      passage = [s] ++ [rest] ++ (drop 1 passage')
   in create xs (result ++ [passage])

dropLast :: [a] -> [a]
dropLast xs =
  let length' = length xs
   in take (length' -1) xs

rhyme :: String
rhyme = unlines . dropLast . lines $ create participants [[]]
