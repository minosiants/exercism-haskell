module ResistorColors (Color (..), value) where

data Color
  = Black
  | Brown
  | Red
  | Orange
  | Yellow
  | Green
  | Blue
  | Violet
  | Grey
  | White
  deriving (Eq, Show)

bandColors :: [(Color, Int)]
bandColors =
  [ (Black, 0),
    (Brown, 1),
    (Red, 2),
    (Orange, 3),
    (Yellow, 4),
    (Green, 5),
    (Blue, 6),
    (Violet, 7),
    (Grey, 8),
    (White, 9)
  ]

value :: (Color, Color) -> Int
value (a, b) =
  case ((findBand a), (findBand b)) of
    (v1, v2) -> read $ (show v1) ++ (show v2)
  where
    findBand v =
      case head (filter (\(c, _) -> c == v) bandColors) of
        (_, band) -> band


-- To complecated 
-- comunity solutions were much simpler 
-- value (a, b) = 10 * fromEnum a + fromEnum b
