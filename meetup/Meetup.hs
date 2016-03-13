module Meetup where 

import Data.Time.Calendar
import Data.Time.Calendar.WeekDate
import Data.Maybe
import Data.List

data Weekday = Monday | Tuesday | Wednesday | Thursday | Friday | Saturday | Sunday deriving (Show ,Eq, Ord, Enum)

data Schedule = First | Second | Third | Fourth | Last | Teenth deriving (Show, Eq)

intToWeekDay :: Int -> Weekday
intToWeekDay n = [Monday ..Sunday] !! (n - 1)

weekdayToInt :: Weekday -> Int
weekdayToInt w = 1 + (fromMaybe (-1) $ elemIndex w [Monday ..Sunday])

meetupDay :: Schedule -> Weekday -> Integer -> Int -> Day
meetupDay s w y m = case s of 
    Teenth -> helper [13..19]
    First  -> helper [1..7]
    Second -> helper [8..14]
    Third  -> helper [15..21]
    Fourth -> helper [22..28]
    Last   -> helper [l-6..l]
    where l = gregorianMonthLength y m
          helper = \days -> fromGregorian y m $ head days + (fromMaybe (-1) . elemIndex w . weekdays $ days)              
          weekdays = map ((\(_,_,wd) -> intToWeekDay wd) . toWeekDate . fromGregorian y m)