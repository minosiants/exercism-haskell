module Proverb (recite) where

recite :: [String] -> String
recite [] = ""
recite xs = mkLine $ init xs `zip` tail xs
  where
    mkLine [] = "And all for the want of a " ++ (head xs) ++ "."
    mkLine ((w1, w2) : xs') =
      "For want of a " ++ w1 ++ " the " ++ w2 ++ " was lost.\n" ++ mkLine xs'
