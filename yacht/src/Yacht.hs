module Yacht (yacht, Category (..)) where

import Data.List

data Category
  = Ones
  | Twos
  | Threes
  | Fours
  | Fives
  | Sixes
  | FullHouse
  | FourOfAKind
  | LittleStraight
  | BigStraight
  | Choice
  | Yacht

sumNum :: Int -> [Int] -> Int
sumNum n = sum . filter (== n)

sameElem :: Int -> [Int] -> Bool
sameElem n xs = (length . nub . take n) xs == 1

yacht :: Category -> [Int] -> Int
yacht Ones xs = sumNum 1 xs
yacht Twos xs = sumNum 2 xs
yacht Threes xs = sumNum 3 xs
yacht Fours xs = sumNum 4 xs
yacht Fives xs = sumNum 5 xs
yacht Sixes xs = sumNum 6 xs
yacht FullHouse dies = check (sort dies)
  where
    check :: [Int] -> Int
    check sdies
      | sameElem 3 sdies && sameElem 2 (reverse sdies) && not (sameElem 5 sdies) = head sdies * 3 + last sdies * 2
      | sameElem 2 sdies && sameElem 3 (reverse sdies) && not (sameElem 5 sdies) = head sdies * 2 + last sdies * 3
      | otherwise = 0
yacht FourOfAKind dies = check (sort dies)
  where
    check :: [Int] -> Int
    check sdies
      | sameElem 4 sdies = head sdies * 4
      | sameElem 4 (reverse sdies) = last sdies * 4
      | otherwise = 0
yacht LittleStraight dies =
  if sort dies == [1 .. 5] then 30 else 0
yacht BigStraight dies =
  if sort dies == [2 .. 6] then 30 else 0
yacht Choice dies = sum dies
yacht Yacht dies = if sameElem 5 dies then 50 else 0
