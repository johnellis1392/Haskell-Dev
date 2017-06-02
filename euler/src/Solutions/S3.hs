
module Solutions.S3 (s3) where

import Celestia.Util.Math (primeFactors)

-- Find the largest prime factor of a given number
s3 :: Int -> Int
s3 = max . primeFactors

