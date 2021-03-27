module RailFenceCipher (encode, decode) where

import Data.Char (isSpace)
import Data.List ( sortOn )
import Data.Maybe ( fromMaybe, isJust )
import Debug.Trace ( trace )

data Direction = Up | Down deriving (Show, Eq)

type Key = Int

type Rail = Int
type Index = Int 

data Step
  = Step
      { direction :: Direction,
        rail :: Rail,
        key :: Key
      }
  deriving (Show)

next :: Step -> Step
next (Step dir i k)
  | i == 1 = Step Down (i + 1) k
  | k == i = Step Up (i - 1) k
  | dir == Down = Step Down (i + 1) k
  | otherwise = Step Up (i -1) k

encode :: Key -> String -> String
encode k txt = orderLetters k $ go (Step Down 1 k) $ filter (not . isSpace) txt

decode :: Key -> String -> String
decode k txt = fromMaybe "" $ mapM snd $  sortOn fst $ concat $ onRails k 1 txt
  where
    railLength = length txt
    onRails k iCaunter txt =
      let resultRail =
            filter (isJust . snd) $
              placeLetters (Step Down 1 k) iCaunter railLength 0 txt
          txt' = drop (length resultRail) txt
       in if iCaunter <= k
            then resultRail : onRails k (iCaunter + 1) txt'
            else []

placeLetters :: Step -> Int -> Int -> Int -> String -> [(Index, Maybe Char)]
placeLetters _ _ _ _ [] = []
placeLetters step railNum railLength lettersCounter (x : xs)
  | railLength == lettersCounter = []
  | rail step == railNum = (lettersCounter, Just x) : placeLetters (next step) railNum railLength (lettersCounter + 1) xs
  | otherwise = placeLetters (next step) railNum railLength (lettersCounter + 1) (x : xs)

go :: Step -> String -> [(Rail, Char)]
go _ [] = []
go step (x : xs) =
  (rail step, x) : go (next step) xs

orderLetters :: Key -> [(Rail, Char)] -> String
orderLetters k letters =
  snd
    <$> concatMap
      ( \i ->
          filter (\(r, _) -> r == i) letters
      )
      [1 .. k]