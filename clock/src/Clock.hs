module Clock (addDelta, fromHourMin, toString) where

type Hours = Int

type Minutes = Int

data Clock = Clock Hours Minutes
  deriving (Eq)

padded :: Int -> String
padded n =
  case (show n) of
    [x] -> "0" ++ [x]
    xs -> xs

instance Show Clock where
  show (Clock hours minutes) = (padded hours) ++ ":" ++ (padded minutes)

toHours :: Int -> Int
toHours h = h `mod` 24

toMinutes :: Int -> (Int, Int)
toMinutes m
  | m < 0 = negative
  | m < 60 = (0, m)
  | otherwise = (hours, minutes)
  where
    hours = m `div` 60
    minutes = m `rem` 60
    negative =
      case minutes of
        0 -> (hours, 0)
        mm -> (hours, 60 + mm)

fromHourMin :: Int -> Int -> Clock
fromHourMin hour minutes =
  let (h, m) = toMinutes minutes
   in Clock (toHours (h + hour)) m

toString :: Clock -> String
toString clock = show clock

addDelta :: Int -> Int -> Clock -> Clock
addDelta hour minutes (Clock h m) =
  fromHourMin (hour + h) (minutes + m)
