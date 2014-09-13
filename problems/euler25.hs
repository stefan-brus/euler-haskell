-- What is the first term in the Fibonacci sequence to contain 1000 digits?

import Data.List
import Data.Maybe

fibs = 1 : fibs' 0 1
  where
    fibs' x1 x2 = let nextfib = x1 + x2 in nextfib : fibs' x2 nextfib

euler25 = let fibnum = head $ filter ((>=1000) . length . show) fibs in fromJust $ fmap (+1) $ elemIndex fibnum fibs
