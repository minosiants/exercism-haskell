module Meetup (Weekday (..), Schedule (..), meetupDay) where

import Data.Time.Calendar (Day, fromGregorian, gregorianMonthLength, toGregorian)
import Data.Time.Calendar.WeekDate (toWeekDate)

data Weekday
  = Monday
  | Tuesday
  | Wednesday
  | Thursday
  | Friday
  | Saturday
  | Sunday
  deriving (Eq, Show, Enum)

data Schedule
  = First
  | Second
  | Third
  | Fourth
  | Last
  | Teenth
  deriving (Eq, Show)

monthDays :: Integer -> Int -> [(Day, Weekday)]
monthDays year month = fmap toDay [1 .. (gregorianMonthLength year month)]
  where
    toDay day =
      let d = fromGregorian year month day
          (_, _, weekday) = toWeekDate d
       in (d, toEnum (weekday -1))


dayNum :: Day -> Int
dayNum d =
      let (_, _, day) = toGregorian d
       in day
meetupDay :: Schedule -> Weekday -> Integer -> Int -> Day
meetupDay schedule weekday year month =
  case schedule of
    First -> head filterWeekDay
    Second -> head . (drop 1) $ filterWeekDay
    Third -> head . (drop 2) $ filterWeekDay
    Fourth -> head . (drop 3) $ filterWeekDay
    Last -> last $ filterWeekDay
    Teenth -> head filterTeenth
  where
    filterTeenth =
      fst
        <$> filter
          (\(d, wd) -> (dayNum d) >= 13 && (dayNum d) <= 19 && wd == weekday)
          days
    filterWeekDay = fst <$> filter ((== weekday) . snd) days
    days = monthDays year month
