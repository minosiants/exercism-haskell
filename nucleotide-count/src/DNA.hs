module DNA (nucleotideCounts, Nucleotide (..)) where

import qualified Data.List as L
import Data.Map
import qualified Data.Map as M

data Nucleotide = A | C | G | T deriving (Eq, Ord, Show)

parse :: Char -> Either Char Nucleotide
parse 'A' = Right A
parse 'C' = Right C
parse 'G' = Right G
parse 'T' = Right T
parse l = Left l

nucleotideCounts :: String -> Either String (Map Nucleotide Int)
nucleotideCounts xs =
  case traverse parse xs of
    Left _ -> Left xs
    Right v -> Right $ L.foldr addToMap M.empty v
      where
        addToMap el =
          insertWith (+) el 1
