module Brackets (arePaired) where

data Bracket = BracketL | BracketR | BracesL | BracesR | ParenthesesL | ParenthesesR deriving (Eq, Show)

mkBracket :: Char -> Maybe Bracket
mkBracket ch =
  case ch of
    '[' -> Just $ BracketL
    ']' -> Just $ BracketR
    '{' -> Just $ BracesL
    '}' -> Just $ BracesR
    '(' -> Just $ ParenthesesL
    ')' -> Just $ ParenthesesR
    _ -> Nothing

arePaired :: String -> Bool
arePaired txt = go [] txt
  where
    go :: [Bracket] -> [Char] -> Bool
    go [] [] = True
    go _ [] = False
    go l@(b : bs) (x : xs) =
      case (b, (mkBracket x)) of
        (BracesL, (Just BracesR)) -> go bs xs
        (_, (Just BracesR)) -> False
        (_, (Just BracesL)) -> go (BracesL : l) xs
        (BracketL, (Just BracketR)) -> go bs xs
        (_, (Just BracketR)) -> False
        (_, (Just BracketL)) -> go (BracketL : l) xs
        (ParenthesesL, (Just ParenthesesR)) -> go bs xs
        (_, (Just ParenthesesR)) -> False
        (_, (Just ParenthesesL)) -> go (ParenthesesL : l) xs
        (_, _) -> go bs xs
