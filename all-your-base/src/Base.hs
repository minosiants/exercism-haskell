module Base (Error (..), rebase) where

import Data.List (find)

data Error a = InvalidInputBase | InvalidOutputBase | InvalidDigit a
  deriving (Show, Eq)

zipWithIndex :: [a] -> [(a, Int)]
zipWithIndex xs = zip xs (reverse [0 .. (length xs) -1])

sum' :: Integral a => [a] -> a
sum' xs = foldr (\(el, i) acc -> el * 10 ^ i + acc) 0 (zipWithIndex xs)

validateDigits :: Integral a => a -> [a] -> Either (Error a) [a]
validateDigits base digits =
  case (find (\n -> n < 0 || n >= base) digits) of
    Nothing -> Right digits
    (Just n) -> Left $ InvalidDigit n

rebase :: Integral a => a -> a -> [a] -> Either (Error a) [a]
rebase inputBase outputBase inputDigits
  | inputBase < 2 = Left InvalidInputBase
  | outputBase < 2 = Left InvalidOutputBase
  | inputBase == 10 = convertOutBase . sum' <$> validated
  | otherwise = convertOutBase . (to10Base inputBase) <$> validated
  where
    validated = validateDigits inputBase inputDigits
    convertOutBase = convert outputBase
    convert _ 0 = []
    convert base num =
      let res = num `div` base
          rest = num `mod` base
       in (convert base res) ++ [rest]
    to10Base base digits =
      sum $
        foldr (\(d, i) acc -> (d * base ^ i) : acc) [] (zipWithIndex digits)
