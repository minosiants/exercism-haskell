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

newtype Reaction = Reaction String deriving (Show)

reactionM :: String -> Maybe Reaction
reactionM str = Just $ Reaction str

data ChainLink
  = ChainLink Species (Maybe Reaction)
  deriving (Show)

foodChain :: [ChainLink]
foodChain =
  [ ChainLink Fly Nothing,
    ChainLink Spider (reactionM "It wriggled and jiggled and tickled inside her."),
    ChainLink Bird (reactionM "How absurd to swallow a bird!"),
    ChainLink Cat (reactionM "Imagine that, to swallow a cat!"),
    ChainLink Dog (reactionM "What a hog, to swallow a dog!"),
    ChainLink Goat (reactionM "Just opened her throat and swallowed a goat!"),
    ChainLink Cow (reactionM "I don't know how she swallowed a cow!"),
    ChainLink Horse Nothing
  ]

verse :: [ChainLink] -> [String]
verse [] = []
verse (x : xs) =
  case x of
    (ChainLink Fly _) ->
      [firstLine Fly] ++ [lastLine] ++ [""] ++ verse xs
    (ChainLink sp (Just reaction)) ->
      let part = printPart sp reaction
       in part ++ verse xs
    (ChainLink Horse _) -> [firstLine Horse] ++ ["She's dead, of course!"]
    _ -> error ""
  where
    printPart :: Species -> Reaction -> [String]
    printPart sp (Reaction r) =
      let uns = uncestors sp foodChain
       in [firstLine sp] ++ [r] ++ fmap secondLine uns ++ [lastLine] ++ [""]

uncestors :: Species -> [ChainLink] -> [(Species, Species)]
uncestors sp xs =
  let ss = fmap (\(ChainLink sp' _) -> sp') xs
      species = takeWhile (/= sp) ss
      s = reverse $ species ++ [sp]
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

song2 = verse foodChain
