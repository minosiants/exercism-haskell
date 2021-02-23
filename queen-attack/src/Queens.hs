module Queens (boardString, canAttack) where

import Data.List

data Side
  = White Int Int
  | Black Int Int
  deriving (Show)

boardString :: Maybe (Int, Int) -> Maybe (Int, Int) -> String
boardString white black =
  case (white, black) of
    (Just (x, y), Nothing) -> format $ place (White x y) board
    (Just (x, y), Just (x', y')) -> format $ place (White x y) $ place (Black x' y') board
    (Nothing, Just (x', y')) -> format $ place (Black x' y') board
    (Nothing, Nothing) -> format board
  where
    place :: Side -> [String] -> [String]
    place side brd =
      case side of
        White x y -> go x y 'W' brd
        Black x y -> go x y 'B' brd
    go :: Int -> Int -> Char -> [String] -> [String]
    go x y ch brd =
      let row = brd !! x
          row' = take y row ++ [ch] ++ drop (y + 1) row
          brd' = take x brd ++ [row'] ++ drop (x + 1) brd
       in brd'

canAttack :: (Int, Int) -> (Int, Int) -> Bool
canAttack (x, y) (x', y')
  | x == x' = True
  | y == y' = True
  | abs (x - x') == abs (y - y') = True
  | otherwise = False

board :: [String]
board = replicate 8 $ replicate 8 '_'

format :: [String] -> String
format xs =
  unlines $
    fmap (intersperse ' ') xs
