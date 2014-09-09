-- The sequence of triangle numbers is generated by adding the natural numbers.
-- So the 7th triangle number would be 1 + 2 + 3 + 4 + 5 + 6 + 7 = 28. The first ten terms would be:
-- 1, 3, 6, 10, 15, 21, 28, 36, 45, 55, ...
-- Let us list the factors of the first seven triangle numbers:
-- 1: 1
-- 3: 1,3
-- 6: 1,2,3,6
--10: 1,2,5,10
--15: 1,3,5,15
--21: 1,3,7,21
--28: 1,2,4,7,14,28
--We can see that 28 is the first triangle number to have over five divisors.
--What is the value of the first triangle number to have over five hundred divisors?

triangle n = sum [1..n]

triangles = map triangle [1..]

nextTriangle n = let parts = triangleFactors 1 in sum $ (last parts) + 1 : parts
  where
    triangleFactors x
      | triangle x == n = [x]
      | otherwise = x : triangleFactors (x + 1)

factors :: Integer -> [Integer]
factors 1 = [1]
factors n = 1 : factors' 2 ++ [n]
  where
    factors' f
      | f > (n `div` 2) = []
      | otherwise = let rest = factors' (f + 1) in if n `mod` f == 0 then f:rest else rest

euler12 = nextTriangle $ last $ takeWhile ((<500) . length . factors) triangles

-- SUPER inefficient, takes years to compute :(

main = putStrLn $ show euler12
