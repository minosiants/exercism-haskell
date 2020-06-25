module Beer (song) where

song :: String
song = go 99 ([99, 98 .. 1])
  where
    go :: Int -> [Int] -> String
    go s [] =
      "No more bottles of beer on the wall, no more bottles of beer.\n\
      \Go to the store and buy some more, "
        ++ show s
        ++ " bottles of beer on the wall.\n"
    go s [_] =
      "1 bottle of beer on the wall, 1 bottle of beer.\n\
      \Take it down and pass it around, no more bottles of beer on the wall.\n\n"
        ++ (go s [])
    go s (x : xs) =
      show bottels ++ " bottles of beer on the wall, " ++ show bottels
        ++ " bottles of beer.\n\
           \Take one down and pass it around, "
        ++ bword
        ++ " of beer on the wall.\n\n"
        ++ go s xs
      where
        bottels = length (x : xs)
        bword = if (bottels -1) == 1 then "1 bottle" else show (bottels -1) ++ " bottles"
