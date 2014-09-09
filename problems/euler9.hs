-- A Pythagorean triplet is a set of three natural numbers, a < b < c, for which,
-- a2 + b2 = c2
-- For example, 32 + 42 = 9 + 16 = 25 = 52.
-- There exists exactly one Pythagorean triplet for which a + b + c = 1000.
-- Find the product abc.

nums = [1..998]

euler9 = head [a * b * c | a <- nums, b <- nums, c <- nums, a + b + c == 1000, (a^2) + (b^2) == (c^2)]

-- Compile and run this as an exe and the computation only takes some seconds
main = putStrLn $ show euler9
