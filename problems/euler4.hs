-- A palindromic number reads the same both ways.
-- The largest palindrome made from the product of two 2-digit numbers is 9009 = 91 × 99.
-- Find the largest palindrome made from the product of two 3-digit numbers.

import Data.List

threeDigits = [999,998..100]

products :: Integer -> [Integer]
products n = map (*n) threeDigits

allProducts = concatMap products threeDigits

isPalindrome :: Integer -> Bool
isPalindrome n = let lst = show n in lst == reverse lst

euler4 = head $ filter isPalindrome $ (reverse . sort) allProducts
