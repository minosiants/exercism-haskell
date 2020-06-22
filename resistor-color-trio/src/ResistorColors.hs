module ResistorColors (Color (..), Resistor (..), label, ohms) where

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
  deriving (Show, Enum, Bounded)

newtype Resistor = Resistor {bands :: (Color, Color, Color)}
  deriving (Show)

label :: Resistor -> String
label resistor
  | result == 0 = "0 ohms"
  | result `mod` 1000000000 == 0 = (show $ result `div` 1000000000) ++ " gigaohms"
  | result `mod` 1000000 == 0 = (show $ result `div` 1000000) ++ " megaohms"
  | result `mod` 1000 == 0 = (show $ result `div` 1000) ++ " kiloohms"
  | otherwise = (show result) ++ " ohms"
  where
    result = ohms resistor

ohms :: Resistor -> Int
ohms (Resistor (c1, c2, c3)) = (10 * (fromEnum c1) + (fromEnum c2)) * (10 ^ (fromEnum c3))
