module Roman (numerals) where

repeat' :: Integer -> a -> [a]
repeat' n el =
  take (fromIntegral n) $ repeat el

thousand :: Integer -> (String, Integer)
thousand n =
  case n `div` 1000 of
    0 -> ("", n)
    x -> ((repeat' x 'M'), n `mod` 1000)

hundred :: Integer -> (String, Integer)
hundred n =
  case n `div` 100 of
    0 -> ("", n)
    x -> ((calc x), n `mod` 100)
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

tens :: Integer -> (String, Integer)
tens n =
  case n `div` 10 of
    0 -> ("", n)
    x -> ((calc x), n `mod` 10)
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

ones :: Integer -> (String, Integer)
ones n = (calc n, 0)
  where
    calc x
      | x < 4 = repeat' n '|'
      | x == 4 = "|V"
      | x == 5 = "V"
      | x == 6 = "V|"
      | x == 7 = "V||"
      | x == 8 = "V|||"
      | x == 9 = "IX"
      | otherwise = ""
