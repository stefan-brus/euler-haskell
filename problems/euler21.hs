-- Let d(n) be defined as the sum of proper divisors of n (numbers less than n which divide evenly into n).
-- If d(a) = b and d(b) = a, where a ≠ b, then a and b are an amicable pair and each of a and b are called amicable numbers.
-- For example, the proper divisors of 220 are 1, 2, 4, 5, 10, 11, 20, 22, 44, 55 and 110; therefore d(220) = 284. The proper divisors of 284 are 1, 2, 4, 71 and 142; so d(284) = 220.
-- Evaluate the sum of all the amicable numbers under 10000.

import Data.List

divisors :: Integer -> [Integer]
divisors 1 = [1]
divisors n = 1 : divisors' 2
  where
    divisors' f
      | f > (n `div` 2) = []
      | otherwise = let rest = divisors' (f + 1) in if n `mod` f == 0 then f:rest else rest

d = sum . divisors

isAmicable :: Integer -> Bool
isAmicable 1 = False
isAmicable n
  | d n == n = False
  | (d . d) n == n = True
  | otherwise = False

euler21 = sum $ filter isAmicable [1..10000]

-- Doesn't take that long to run, honest! But compilation sure speeds it up

main = print euler21
