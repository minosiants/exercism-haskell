{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
module WordProblem (answer) where
import Debug.Trace
import Control.Applicative
import Data.Char

data Operation
  = Plus
  | Minus
  | Multiply
  | Divide
  deriving (Show, Eq)


parse :: String -> Maybe Integer
parse str =
  case words str of
    ("What" : "is" : xs) ->
      number' (head (trace("xs: "++ show xs)(xs))) <|> expression xs
    _ -> Nothing

operation :: Operation -> Integer -> Integer -> Integer
operation op x y =
  case op of
    Plus -> x + y
    Minus -> x - y
    Multiply -> x * y
    Divide -> x `div` y

expression :: [String] -> Maybe Integer
expression xs =
  if length xs < 3
    then Nothing
    else do
      n1 <- parseNumber (head xs)
      op <- parseOperation (xs !! 1)
      operation op n1 <$>
        ( if op == Multiply || op == Divide
            then parseNumber (xs !! 3)
            else
              parseNumber
                ( xs !! 2
                )
          )

parseOperation :: String -> Maybe Operation
parseOperation str
  | str == "plus" = Just Plus
  | str == "minus" = Just Minus
  | str == "divided" = Just Divide
  | str == "multiplied" = Just Multiply
  | otherwise = Nothing

number' :: String -> Maybe Integer
number' xs =
  let s = if last xs == '?'
            then parseNumber xs
            else Nothing
  in trace ("num: "++ show s ++ "xs: " ++show xs)s   

parseNumber :: String -> Maybe Integer
parseNumber n@(x:xs)
  | x == '-' =fmap  (\x -> x * (-1)) (go xs)
  | otherwise = go n
  where 
    go xs = 
        let r = takeWhile isNumber xs
        in if null r then Nothing else Just $ read r

answer :: String -> Maybe Integer
answer = parse
