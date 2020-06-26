module Garden
  ( Plant (..),
    garden,
    lookupPlants,
  )
where

import Data.List
import Data.Maybe

data Plant
  = Clover
  | Grass
  | Radishes
  | Violets
  deriving (Eq, Show)

type Student = String

data Garden = Garden [(Student, [Plant])]

toPlant :: Char -> Maybe Plant
toPlant ch =
  case ch of
    'C' -> Just Clover
    'G' -> Just Grass
    'R' -> Just Radishes
    'V' -> Just Violets
    _ -> Nothing

garden :: [String] -> String -> Garden
garden students plants = Garden $ fromMaybe [] $ go (sort students) (rows (head lp) (last lp))
  where
    go :: [String] -> [String] -> Maybe [(Student, [Plant])]
    go [] _ = Just []
    go _ [] = Just []
    go (x : xs) (r : rx) =
      ( fmap (\p -> [(x, p)]) $
          traverse toPlant r
      )
        <> go xs rx
    lp = lines plants
    rows [] [] = []
    rows xs ys = [(take 2 xs) ++ (take 2 ys)] ++ rows (drop 2 xs) (drop 2 ys)

lookupPlants :: String -> Garden -> [Plant]
lookupPlants student (Garden students) = fromMaybe [] $ fmap snd $ find (\(s, _) -> s == student) students
