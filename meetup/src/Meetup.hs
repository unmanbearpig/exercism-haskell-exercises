module Meetup (Weekday(..), Schedule(..), meetupDay) where

import Data.Time.Calendar (Day, fromGregorian, toGregorian, gregorianMonthLength)
import Data.Time.Calendar.WeekDate (toWeekDate)

data Weekday = Sunday
             | Monday
             | Tuesday
             | Wednesday
             | Thursday
             | Friday
             | Saturday deriving (Eq, Show, Enum)

data Schedule = First
              | Second
              | Third
              | Fourth
              | Last
              | Teenth

weekday :: Int -> Weekday
weekday x = toEnum $ x `mod` 7

dayWeekday :: Day -> Weekday
dayWeekday day = weekday weekdayNum
  where (_, _, weekdayNum) = toWeekDate day

days :: [Int] -> Integer -> Int -> [Day]
days ds y m = map (fromGregorian y m) ds

monthDays :: Integer -> Int -> [Day]
monthDays y m = days [1..monthLength] y m
  where monthLength = gregorianMonthLength y m

pickScheduleDay :: Schedule -> [a] -> a
pickScheduleDay Last    = last
pickScheduleDay Teenth  = flip (!!) 0
pickScheduleDay First   = flip (!!) 0
pickScheduleDay Second  = flip (!!) 1
pickScheduleDay Third   = flip (!!) 2
pickScheduleDay Fourth  = flip (!!) 3

scheduleDays :: Schedule -> Integer -> Int -> [Day]
scheduleDays Teenth y m = drop 12 $ monthDays y m
scheduleDays _      y m = monthDays y m

dayWeekdays :: [Day] -> [(Day, Weekday)]
dayWeekdays = map (\d -> (d, (dayWeekday d)))

meetupDay :: Schedule -> Weekday -> Integer -> Int -> Day
meetupDay sched wd y m =
  pickScheduleDay sched .
  map fst .
  filter ((==) wd . snd) .
  dayWeekdays $ scheduleDays sched y m
