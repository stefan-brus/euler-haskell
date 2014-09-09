-- The prime factors of 13195 are 5, 7, 13 and 29.
-- What is the largest prime factor of the number 600851475143 ?

divisors :: Integer -> [Integer]
divisors n | n <= 1 = []
divisors n = let divisor = firstDivisor n 2 in divisor : divisors (n `div` divisor)
  where
    firstDivisor n x
      | n == x = n
      | n `mod` x == 0 = x
      | otherwise = firstDivisor n (x + 1)

euler3 = last $ divisors 600851475143
