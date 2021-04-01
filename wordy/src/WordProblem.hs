module WordProblem (answer) where

import Data.Char (isNumber)
import Data.Maybe (isJust, maybeToList)
import Debug.Trace (trace)

data Operation
  = Plus
  | Minus
  | Multiply
  | Divide
  | Unsupported
  deriving (Show, Eq)

data WordType
  = Ops
  | Num
  deriving (Eq, Show)

isTwoWordsOp :: Operation -> Bool
isTwoWordsOp op =
  case op of
    Divide -> True
    Multiply -> True
    _ -> False

parseAll :: String -> Maybe Integer
parseAll str =
  case words str of
    ("What" : "is" : xs) ->
      parse xs
    _ -> Nothing

parse :: [String] -> Maybe Integer
parse xs
  | Unsupported `elem` ops = Nothing
  | null ops && null nums = Nothing
  | null ops = Just $ head nums
  | otherwise = Just $ go nums ops
  where
    nums = parseNumbers xs
    ops = parseOperations xs
    go :: [Integer] -> [Operation] -> Integer
    go xs op =
      fst $
        foldl
          ( \(acc, opp) x ->
              let res = operation (head opp) acc x
               in (res, tail opp)
          )
          (head xs, op)
          (tail xs)

operation :: Operation -> Integer -> Integer -> Integer
operation op x y =
  case op of
    Plus -> x + y
    Minus -> x - y
    Multiply -> x * y
    Divide -> x `div` y
    Unsupported -> error ""

parseNumbers :: [String] -> [Integer]
parseNumbers xs = concat $ maybeToList $sequence $ filter isJust $ fmap parseNumber xs

parseOperations :: [String] -> [Operation]
parseOperations xs = go xs Num
  where
    go [] _ = []
    go (x : xs) wt =
      case wt of
        Ops ->
          let op = parseOperation x
              xs' = if isTwoWordsOp op then tail xs else xs
           in op : go xs' Num
        Num -> go xs Ops

parseOperation :: String -> Operation
parseOperation str
  | str == "plus" = Plus
  | str == "minus" = Minus
  | str == "divided" = Divide
  | str == "multiplied" = Multiply
  | otherwise = Unsupported

parseNumber :: String -> Maybe Integer
parseNumber n@(x : xs)
  | x == '-' = fmap (* (-1)) (go xs)
  | otherwise = go n
  where
    go xs =
      let r = takeWhile isNumber xs
       in if null r then Nothing else Just $ read r

answer :: String -> Maybe Integer
answer = parseAll
