
module Solutions.S10 (s10) where

import Celestia.Util.Math (sieveOfEratosthenes')

-- 

s10 :: Integer -> Integer
s10 n = sum . sieveOfEratosthenes' $ n


-- Solution:
-- s10 == (?)



