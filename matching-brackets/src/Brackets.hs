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

arePaired :: [Char] -> Bool
arePaired txt = go [] txt
  where
    go :: [Bracket] -> [Char] -> Bool
    go [] [] = True
    go _ [] = False
    go bs (x : xs) =
      case (bs, (mkBracket x)) of
        (BracesL : bs', (Just BracesR)) -> go bs' xs
        (_, (Just BracesR)) -> False
        (_, (Just BracesL)) -> go (BracesL : bs) xs
        (BracketL : bs', (Just BracketR)) -> go bs' xs
        (_, (Just BracketR)) -> False
        (_, (Just BracketL)) -> go (BracketL : bs) xs
        (ParenthesesL : bs', (Just ParenthesesR)) -> go bs' xs
        (_, (Just ParenthesesR)) -> False
        (_, (Just ParenthesesL)) -> go (ParenthesesL : bs) xs
        (_, Nothing) -> go bs xs
