
module Solutions.S1 (s1) where

-- Calculate the product of all multiples of 3 and 5 from 1 through 1000
s1 :: Integer -> Integer
s1 n = sum . filter multiples $ [1..n - 1]
  where
  multiples n = (multipleOf 3 n) || (multipleOf 5 n)
  multipleOf i n = n `mod` i == 0

-- Solution:
-- s1 1000 = 233168


