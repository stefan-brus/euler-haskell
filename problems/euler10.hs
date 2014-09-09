-- The sum of the primes below 10 is 2 + 3 + 5 + 7 = 17.
-- Find the sum of all the primes below two million.

initPrimes = [2]

divisibleByNone :: Integer -> [Integer] -> Bool
divisibleByNone n xs = not $ any (\x -> n `mod` x == 0) xs

nextPrime :: Integer -> [Integer] -> Integer
nextPrime n xs
    | divisibleByNone n xs = n
    | otherwise = nextPrime (n + 2) xs

primes = primes' 3 initPrimes
  where
    primes' n xs = let next = nextPrime n xs in if next == n then n:primes' (n + 2) (n:xs) else primes' (n + 2) xs

euler10 = sum $ takeWhile (<200000) primes

-- This is super slow, even when compiled, I'm so sorry
main = putStrLn $ show euler10
