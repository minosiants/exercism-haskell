module Frequency (frequency) where

import qualified Data.Char as C
import Data.Map (Map)
import qualified Data.Map as M
import Data.Text (Text)
import qualified Data.Text as T

frequency :: Int -> [Text] -> Map Char Int
frequency nWorkers texts =
  T.foldr addChar M.empty (T.strip . T.toLower $ T.concat texts)
  where
    addChar :: Char -> Map Char Int -> Map Char Int
    addChar ch m
      | not (C.isLetter ch) = m
      | otherwise =
        case M.lookup ch m of
          Nothing -> M.insert ch 1 m
          (Just n) -> M.insert ch (n + 1) m

-- https://exercism.io/tracks/haskell/exercises/parallel-letter-frequency/solutions/71f74f058ca34c4ca486511d9963ab4e
-- Neat solution

-- | frequency :: Int -> [Text] -> Map Char Int
--    frequency _ texts = (Map.fromListWith (+)) . (concatMap mapPair) $ cleanToStrings texts
--      where cleanToStrings = map (Text.unpack . (Text.map toLower) . (Text.filter isLetter))
--          mapPair = map (\c -> (c,1))
