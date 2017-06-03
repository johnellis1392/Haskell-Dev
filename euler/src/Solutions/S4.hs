
module Solutions.S4 (s4) where

import Control.Monad (liftM2)
-- import Data.List ()
-- import Control.Applicative ()

-- Find the largest palindrome product (ex. 91 * 99 == 9009) 
-- for two n-digit numbers.

s4 :: Int -> Int
s4 n = let xs = [1..10^n-1]
  in foldl1 max .
    filter (\i -> show i == (reverse . show $ i)) .
    fmap (uncurry (*)) $ liftM2 (,) xs xs


-- Solution:
-- s4 3 == 906609


