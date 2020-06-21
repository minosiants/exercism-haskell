module Diamond (diamond) where

import Data.Char (isAlpha)

getLetters :: Char -> [Char] -> [Char]
getLetters ch letters =
  (takeWhile (/= ch) letters) ++ [ch]

diamond :: Char -> Maybe [String]
diamond ch
  | not $ isAlpha ch = Nothing
  | otherwise = Just $ half ++ (drop 1 $ reverse half)
  where
    half = createHalf 0 $ getLetters ch ['A' .. 'Z']
    createHalf :: Int -> [Char] -> [String]
    createHalf i letters
      | i == 0 = [(space $ size letters) ++ [letters !! i] ++ (space $ size letters)] ++ createHalf (i + 1) letters
      | i == (length letters) = []
      | otherwise =
        [ spaceWithChar (letters !! i) (i -1) 0 (size letters)
            ++ " "
            ++ (reverse $ spaceWithChar (letters !! i) (i -1) 0 (size letters))
        ]
          ++ (createHalf (i + 1) letters)
    size l = length l -1
    space s = take s $ repeat ' '
    spaceWithChar :: Char -> Int -> Int -> Int -> String
    spaceWithChar _ch pos i s
      | i == s = []
      | pos == i = (spaceWithChar _ch pos (i + 1) s) ++ [_ch]
      | otherwise = (spaceWithChar _ch pos (i + 1) s) ++ " "
