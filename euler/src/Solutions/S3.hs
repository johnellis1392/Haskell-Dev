
module Solutions.S3 (s3) where

import Debug.Trace (trace)
import Celestia.Util.Math (primeFactors, euclidean_gcd)

-- Find the largest prime factor of a given number
s3 :: Integer -> Integer
s3 = last . primeFactors -- This works since the factor list is already sorted
-- s3 = foldl max 1 . primeFactors


