module Minesweeper (annotate) where

import Data.Maybe ( fromMaybe, isJust )
import Data.List ( unfoldr )

annotate :: [String] -> [String]
annotate [""] = [""]
annotate board
  | (filter (isBomb) $ concat board) == [] = board
  | otherwise =
    group' xMax $ concat (go 0 0)
  where
    go :: Int -> Int -> [String]
    go x y
      | x == xMax = go 0 (y + 1)
      | y == yMax = []
      | otherwise =
         case square x y of
          Nothing -> "" : go (x + 1) y
          Just '*' -> "*" : go (x+1) y 
          Just ch ->
            let squares =
                  [ square (x + 1) y,
                    square (x -1) y,
                    square x (y + 1),
                    square x (y -1),
                    square (x + 1) (y + 1),
                    square (x + 1) (y -1),
                    square (x -1) (y -1),
                    square (x -1) (y + 1)
                  ]
                r = length $ filter (isBomb) $fromMaybe [] $ sequence $ filter isJust squares
                res = if r > 0  then show r else [ch]
            in res : go (x +1) y


    yMax = length board
    xMax = length $ head board
    isBomb ch = ch == '*'
    square x y
      | x < 0 || y < 0 = Nothing  
      | x >= xMax = Nothing
      | y >= yMax = Nothing 
      | otherwise =
        Just $ (board !! y) !!x  

group' :: Int -> String -> [String]
group' n = unfoldr (\x ->
  if (length x) == 0
    then Nothing
    else Just $((take n x), drop n x))
