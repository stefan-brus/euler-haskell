-- If the numbers 1 to 5 are written out in words: one, two, three, four, five, then there are 3 + 3 + 5 + 4 + 4 = 19 letters used in total.
-- If all the numbers from 1 to 1000 (one thousand) inclusive were written out in words, how many letters would be used?

ones :: Char -> String
ones '0' = ""
ones '1' = "one"
ones '2' = "two"
ones '3' = "three"
ones '4' = "four"
ones '5' = "five"
ones '6' = "six"
ones '7' = "seven"
ones '8' = "eight"
ones '9' = "nine"

tens :: Char -> String
tens '0' = ""
tens '2' = "twenty"
tens '3' = "thirty"
tens '4' = "forty"
tens '5' = "fifty"
tens '6' = "sixty"
tens '7' = "seventy"
tens '8' = "eighty"
tens '9' = "ninety"

teens :: String -> String
teens "10" = "ten"
teens "11" = "eleven"
teens "12" = "twelve"
teens "13" = "thirteen"
teens "14" = "fourteen"
teens "15" = "fifteen"
teens "16" = "sixteen"
teens "17" = "seventeen"
teens "18" = "eighteen"
teens "19" = "nineteen"

numToStr :: Integer -> String
numToStr n
  | (n >= 100) && ((n `mod` 100) == 0) = ones (head str) ++ "hundred"
  | n >= 100 = ones (head str) ++ "hundredand" ++ numToStr ((read . tail) str)
  | n >= 20 = tens (head str) ++ numToStr ((read . tail) str)
  | n >= 10 = teens str
  | otherwise = ones (head str)
    where
      str = show n

euler17 = (length "onethousand") + (sum $ map (length . numToStr) [1..999])
