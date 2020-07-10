module Base (Error (..), rebase) where

import Data.List (find)
import Debug.Trace

data Error a = InvalidInputBase | InvalidOutputBase | InvalidDigit a
  deriving (Show, Eq)

sum' :: Integral a => [a] -> a
sum' n =
  let zipped = zip n (reverse [0 .. (length n) -1])
   in foldr (\(el, i) acc -> el * 10 ^ i + acc) 0 zipped

validateDigits :: Integral a => a -> [a] -> Either (Error a) [a]
validateDigits base digits =
  case (find (\n -> n < 0 || n >= base) digits) of
    Nothing -> Right digits
    (Just n) -> Left $ InvalidDigit n

rebase :: Integral a => a -> a -> [a] -> Either (Error a) [a]
rebase inputBase outputBase inputDigits
  | inputBase < 2 = Left InvalidInputBase
  | outputBase < 2 = Left InvalidOutputBase
  | inputBase == 10 = (convert outputBase) . sum' <$> (validateDigits inputBase inputDigits)
  | otherwise = (convert outputBase) . (to10Base inputBase) <$> (validateDigits inputBase inputDigits)
  where
    convert _ 0 = []
    convert base num =
          let res = num `div` base
              rest = num `mod` base
           in (convert base res) ++ [rest]
    to10Base base digits =
      let zipped = zip digits $ reverse [0 .. (length digits) - 1]
       in sum $
            foldr (\(d, i) acc -> (d * base ^ i) : acc) [] zipped
