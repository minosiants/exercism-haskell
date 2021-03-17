module Palindromes (largestPalindrome, smallestPalindrome) where

import Data.List
import qualified Data.MultiMap as M

largestPalindrome :: Integer -> Integer -> Maybe (Integer, [(Integer, Integer)])
largestPalindrome = palindrome last

smallestPalindrome :: Integer -> Integer -> Maybe (Integer, [(Integer, Integer)])
smallestPalindrome = palindrome head

type Key = [Integer] -> Integer

palindrome :: Key -> Integer -> Integer -> Maybe (Integer, [(Integer, Integer)])
palindrome key minFactor maxFactor
  | null keys = Nothing
  | otherwise = case M.lookup (key keys) r of
    [] -> Nothing
    v -> Just $ (key keys, v)
  where
    r = M.fromList $ filter (isPalendrom . fst) [(x * y, (x, y)) | x <- [minFactor .. maxFactor], y <- [minFactor .. maxFactor]]
    keys = sort $ M.keys r
    isPalendrom i = show i == reverse (show i)
