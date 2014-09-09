-- You are given the following information, but you may prefer to do some research for yourself.
-- 1 Jan 1900 was a Monday.
-- Thirty days has September,
-- April, June and November.
-- All the rest have thirty-one,
-- Saving February alone,
-- Which has twenty-eight, rain or shine.
-- And on leap years, twenty-nine.
-- A leap year occurs on any year evenly divisible by 4, but not on a century unless it is divisible by 400.
-- How many Sundays fell on the first of the month during the twentieth century (1 Jan 1901 to 31 Dec 2000)?

-- number of days = 365 * 100 plus one day for each leap year

numDays = 365 * 100 + (100 `div` 4)

weekdays = map ((1+) . (`mod` 7)) [1..numDays]

daysInMonth :: Integer -> Integer -> Integer
daysInMonth m y
  | m == 1 || m == 3 || m == 5 || m == 7 || m == 8 || m == 10 || m == 12 = 31
  | m == 4 || m == 6 || m == 9 || m == 11 = 30
  | y `mod` 4 == 0 = 29
  | otherwise = 28

firstWeekdays :: [Integer]
firstWeekdays = firstWeekdays' 1 1 1
  where
    firstWeekdays' d _ _ | d > numDays = []
    firstWeekdays' d m y = weekdays !! (fromInteger d) : firstWeekdays' (d + (daysInMonth m y)) nextMonth nextYear
      where
        nextMonth = if m == 12 then 1 else m + 1
        nextYear = if m == 12 then y + 1 else y

euler19 = length $ filter (==7) $ take (fromInteger numDays) firstWeekdays
