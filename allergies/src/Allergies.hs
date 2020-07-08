module Allergies (Allergen (..), allergies, isAllergicTo) where

data Allergen
  = Eggs
  | Peanuts
  | Shellfish
  | Strawberries
  | Tomatoes
  | Chocolate
  | Pollen
  | Cats
  deriving (Eq, Show, Enum)

log' :: Int -> Float
log' n = logBase 2 $ fromIntegral n

allergen :: Int -> [Allergen]
allergen i
  | i < 8 && i >= 0 = [toEnum i]
  | otherwise = []

allergies :: Int -> [Allergen]
allergies score
  | score == 0 = []
  | score == 1 = [Eggs]
  | otherwise = findAlergens score
  where
    findAlergens s =
      let logarithm = log' s
          fractional = floor logarithm
          score' = 2 ^ fractional
       in allergies (s - score') ++ (allergen fractional)

isAllergicTo :: Allergen -> Int -> Bool
isAllergicTo a score = a `elem` allergies score
