module DNA (nucleotideCounts, Nucleotide (..)) where

import qualified Data.List as L
import Data.Map
import qualified Data.Map as M

data Nucleotide = A | C | G | T deriving (Eq, Ord, Show)

parce :: Char -> Either Char Nucleotide
parce 'A' = Right A
parce 'C' = Right C
parce 'G' = Right G
parce 'T' = Right T
parce l = Left l

nucleotideCounts :: String -> Either String (Map Nucleotide Int)
nucleotideCounts xs =
  case traverse parce xs of
    Left _ -> Left xs
    Right v -> Right $ L.foldr addToMap M.empty v
      where
        addToMap el m =
          insertWith (+) el 1 m
