-- Starting in the top left corner of a 2×2 grid, and only being able to move to the right and down, there are exactly 6 routes to the bottom right corner.
-- How many such routes are there through a 20×20 grid?

-- 0 represents down, 1 represents right - the possible moves of a 1x1 grid

import Data.List

moves = [0..1]

grid :: Integer -> [Integer]
grid 1 = moves
grid n = moves ++ grid (n - 1)

euler15 = (length . nub . permutations . grid) 20

-- Amazingly slow, TODO: See if this is optimizable with Data.Set or something

main = putStrLn $ show euler15
