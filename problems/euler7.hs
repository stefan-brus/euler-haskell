-- By listing the first six prime numbers: 2, 3, 5, 7, 11, and 13, we can see that the 6th prime is 13.
-- What is the 10 001st prime number?

divisors :: Integer -> [Integer]
divisors n | n <= 1 = []
divisors n = let divisor = firstDivisor n 2 in divisor : divisors (n `div` divisor)
  where
    firstDivisor n x
      | n == x = n
      | n `mod` x == 0 = x
      | otherwise = firstDivisor n (x + 1)

isPrime n = (length $ divisors n) == 1

nextPrime n = if isPrime n then n else nextPrime (n + 2)

allPrimes :: [Integer]
allPrimes = 2 : allPrimes' 3
  where allPrimes' n = let next = nextPrime n in next : allPrimes' (next + 2)

euler7 = last $ take 10001 allPrimes

-- takes about a minute to run when compiled, not the most optimal solution

main = putStrLn $ show euler7
