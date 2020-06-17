module DNA (toRNA) where

complement :: Char -> Either Char Char
complement ch =
  case ch of
    'G' -> Right 'C'
    'C' -> Right 'G'
    'T' -> Right 'A'
    'A' -> Right 'U'
    _ -> Left ch

toRNA :: String -> Either Char String
toRNA = traverse complement
