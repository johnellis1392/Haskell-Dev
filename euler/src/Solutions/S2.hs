module Solutions.S2 (s2) where
-- import Celestia.Util.Math (fibonacci)
import Celestia.Util (fibonacci)


-- Find the sum of the first n digits of the fibonacci sequence.
-- Note: this test expects the first two digits to be dropped.
--       ie: s2 10 == sum [1,2,3,5,...]
s2 :: Int -> Integer
s2 limit = sum . evens $ take limit . drop 2 $ fibonacci
  where
  evens = filter even
  even i = i `mod` 2 == 0

