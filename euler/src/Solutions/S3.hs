
module Solutions.S3 (s3) where

import Debug.Trace (trace)
import Celestia.Util.Math (primeFactors, euclidean_gcd)

-- Find the largest prime factor of a given number
-- This works since the factor list is already sorted
s3 :: Integer -> Integer
s3 = last . primeFactors

-- Alternative solution:
-- s3 = foldl max 1 . primeFactors

-- Solution:
-- s3 600851475143 == 6857

