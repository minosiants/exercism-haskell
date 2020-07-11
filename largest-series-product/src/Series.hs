module Series (Error (..), largestProduct) where

import Control.Monad (foldM)
import Data.Char (digitToInt, isDigit)

data Error = InvalidSpan | InvalidDigit Char deriving (Show, Eq)

parse :: Char -> Either Error Integer
parse digit
  | isDigit digit = Right $ (fromIntegral . digitToInt) digit
  | otherwise = Left $ InvalidDigit digit

largestProduct :: Int -> String -> Either Error Integer
largestProduct size digits
  | size < 0 || length digits < size = Left InvalidSpan
  | size == 0 && length digits == 0 = Right 1
  | size == 0 = parse (head digits)
  | otherwise = calculate
  where
    start = fmap (\s -> (s, (drop size digits))) $ traverse parse $ take size digits
    process (sizedDigits, result) newDight = do
      d <- parse newDight  
      let sizedDigits' = (drop 1 sizedDigits) ++ [d]
          biggest' = product $ sizedDigits'
       in return $ (sizedDigits', max biggest' result)
    calculate = do
      (s, rest) <- start
      (_, result) <- foldM process (s, product s) rest
      return $ result
