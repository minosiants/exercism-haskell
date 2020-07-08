module Roman (numerals) where

import Control.Monad.Trans.State

numerals :: Integer -> Maybe String
numerals n =
  let r = concat <$> sequence [thousand, hundred, tens, ones]
   in result $ evalState r n
  where
    result :: String -> Maybe String
    result [] = Nothing
    result xs = Just xs


repeat' :: Integer -> a -> [a]
repeat' n el =
  take (fromIntegral n) $ repeat el

updateState :: (Integer -> (String, Integer)) -> State Integer String
updateState f = do
  n <- get
  let (str, ns) = f n
  put ns
  return str

thousand :: State Integer String
thousand =
  updateState
    ( \n ->
        case n `div` 1000 of
          0 -> ("", n)
          x -> ((repeat' x 'M'), n `mod` 1000)
    )

hundred :: State Integer String
hundred =
  updateState
    ( \n ->
        case n `div` 100 of
          0 -> ("", n)
          x -> ((calc x), n `mod` 100)
    )
  where
    calc x
      | x < 4 = repeat' x 'C'
      | x == 4 = "CD"
      | x == 5 = "D"
      | x == 6 = "DC"
      | x == 7 = "DCC"
      | x == 8 = "DCCC"
      | x == 9 = "CM"
      | otherwise = ""

tens :: State Integer String
tens =
  updateState
    ( \n ->
        case n `div` 10 of
          0 -> ("", n)
          x -> ((calc x), n `mod` 10)
    )
  where
    calc x
      | x < 4 = repeat' x 'X'
      | x == 4 = "XL"
      | x == 5 = "L"
      | x == 6 = "LX"
      | x == 7 = "LXX"
      | x == 8 = "LXXX"
      | x == 9 = "XC"
      | otherwise = ""

ones :: State Integer String
ones =
  updateState
    ( \n ->
        (calc n, 0)
    )
  where
    calc x
      | x < 4 = repeat' x 'I'
      | x == 4 = "IV"
      | x == 5 = "V"
      | x == 6 = "VI"
      | x == 7 = "VII"
      | x == 8 = "VIII"
      | x == 9 = "IX"
      | otherwise = ""
