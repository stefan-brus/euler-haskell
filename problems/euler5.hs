-- 2520 is the smallest number that can be divided by each of the numbers from 1 to 10 without any remainder.
-- What is the smallest positive number that is evenly divisible by all of the numbers from 1 to 20?

divisibleByAll :: Integer -> Bool
divisibleByAll n = all (\x -> n `mod` x == 0) [1..20]

euler5 = head $ filter divisibleByAll [40,60..]
