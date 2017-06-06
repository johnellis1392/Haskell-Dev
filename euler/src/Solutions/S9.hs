
module Solutions.S9 (s9) where

import Debug.Trace (traceShowId)
import Data.Maybe (fromJust)
import Data.List (find)

-- Get the special pythagorean triplet satisfying the following properties:
-- Given some a, b, and c,
-- 1) a < b < c
-- 2) a^2 + b^2 == c^2
-- 3) a + b + c == 1000

s9 :: Int
s9 = (\(a, b, c) -> a * b * c) . traceShowId . fromJust . find constraints $ pythagoreanTriplets

  where

  pythagoreanTriplets :: [(Int, Int, Int)]
  pythagoreanTriplets = [(a, b, c) | c <- [3..], b <- [2..c], a <- [1..b]]

  constraints :: (Int, Int, Int) -> Bool
  constraints (a, b, c) = (a ^ 2 + b ^ 2 == c ^ 2) && (a + b + c == 1000)


-- Solution:
-- s9 == 31875000


