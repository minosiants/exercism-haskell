module ProteinTranslation (proteins) where

import Data.List

data Codon
  = AUG
  | UUU
  | UUC
  | UUA
  | UUG
  | UCU
  | UCC
  | UCA
  | UCG
  | UAU
  | UAC
  | UGU
  | UGC
  | UGG
  | UAA
  | UAG
  | UGA
  deriving (Enum, Eq, Show)

data Protein
  = Methionine
  | Phenylalanine
  | Leucine
  | Serine
  | Tyrosine
  | Cysteine
  | Tryptophan
  | STOP
  deriving (Enum, Eq, Show)

codon :: String -> Maybe Codon
codon s =
  case s of
    "AUG" -> Just AUG
    "UUU" -> Just UUU
    "UUC" -> Just UUC
    "UUA" -> Just UUA
    "UUG" -> Just UUG
    "UCU" -> Just UCU
    "UCC" -> Just UCC
    "UCA" -> Just UCA
    "UCG" -> Just UCG
    "UAU" -> Just UAU
    "UAC" -> Just UAC
    "UGU" -> Just UGU
    "UGC" -> Just UGC
    "UGG" -> Just UGG
    "UAA" -> Just UAA
    "UAG" -> Just UAG
    "UGA" -> Just UGA
    _ -> Nothing

findProtein :: Codon -> Maybe Protein
findProtein c
  | c == AUG = Just Methionine
  | elem c [UUU, UUC] = Just Phenylalanine
  | elem c [UUA, UUG] = Just Leucine
  | elem c [UCU, UCC, UCA, UCG] = Just Serine
  | elem c [UAU, UAC] = Just Tyrosine
  | elem c [UGU, UGC] = Just Cysteine
  | c == UGG = Just Tryptophan
  | elem c [UAA, UAG, UGA] = Just STOP
  | otherwise = Nothing

proteins :: String -> Maybe [String]
proteins cod = do
  c <- traverse codon $ codons cod
  p <- traverse findProtein c
  return $ fmap show $ takeWhile (/= STOP) p
  where
    codons = takeWhile (not . null) . unfoldr (Just . splitAt 3)
