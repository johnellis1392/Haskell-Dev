
module Solutions.S7 (s7) where

-- Get the nth prime number

import Celestia.Util.Math (sieveOfEratosthenes)

s7 :: Int -> Integer
s7 n = head . take 1 . drop (n - 1) $ sieveOfEratosthenes


-- Solution:
-- s7 10001 == 104743

