-- Calculate the product of all multiples of 3 and 5 from 1 through 1000
module Solutions.S1 (s1) where

s1 :: Integer -> Integer
s1 upperBound = product . multiples $ upTo upperBound
  where
  multiples = filter isMultiple
  isMultiple i = multipleOf 3 i || multipleOf 5 i
  multipleOf i n = n `mod` i == 0
  upTo n = [1..n - 1]


