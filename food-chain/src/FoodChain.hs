module FoodChain (song) where

data Species
  = Fly
  | Spider
  | Bird
  | Cat
  | Dog
  | Goat
  | Cow
  | Horse
  deriving (Eq)

instance Show Species where
  show Fly = "fly"
  show Spider = "spider"
  show Bird = "bird"
  show Cat = "cat"
  show Dog = "dog"
  show Goat = "goat"
  show Cow = "cow"
  show Horse = "horse"

class React a where
  react :: a -> String

instance React Species where
  react Spider = "It wriggled and jiggled and tickled inside her."
  react Bird = "How absurd to swallow a bird!"
  react Cat = "Imagine that, to swallow a cat!"
  react Dog = "What a hog, to swallow a dog!"
  react Goat = "Just opened her throat and swallowed a goat!"
  react Cow = "I don't know how she swallowed a cow!"
  react _ = ""

foodChain :: [Species]
foodChain =
  [ Fly,
    Spider,
    Bird,
    Cat,
    Dog,
    Goat,
    Cow,
    Horse
  ]

verse :: [Species] -> [String]
verse [] = []
verse (x : xs) =
  case x of
    Fly -> [firstLine Fly] ++ [lastLine] ++ [""] ++ verse xs
    Horse -> firstLine Horse : ["She's dead, of course!"]
    sp -> printVerse sp ++ verse xs

printVerse :: Species -> [String]
printVerse sp =
  let uns = uncestors sp foodChain
   in [firstLine sp] ++ [react sp] ++ fmap secondLine uns ++ [lastLine] ++ [""]

uncestors :: Species -> [Species] -> [(Species, Species)]
uncestors sp xs =
  let s = reverse $ takeWhile (/= sp) xs ++ [sp]
   in zip s (tail s)

secondLine :: (Species, Species) -> String
secondLine (sp1, sp2) =
  let mainPart = "She swallowed the " ++ show sp1 ++ " to catch the " ++ show sp2
   in case sp2 of
        Spider -> mainPart ++ " that wriggled and jiggled and tickled inside her."
        _ -> mainPart ++ "."

firstLine :: Species -> String
firstLine sp = "I know an old lady who swallowed a " ++ show sp ++ "."

lastLine :: String
lastLine = "I don't know why she swallowed the fly. Perhaps she'll die."

song :: String
song = unlines $ verse foodChain
