
module Solutions.S5 (s5) where

import Data.List (iterate, find)
import Data.Maybe (fromJust)

-- Calculate the least number that is divisible
-- by all numbers from 1 to n

s5 :: Int -> Int
s5 n = let xs = [1..n]
  in fromJust . find (\i -> all (divisible i) xs) . iterate (+n) $ n

  where

  divisible :: Int -> Int -> Bool
  divisible i j = i `mod` j == 0


-- Solution:
-- s5 20 == 232792560


