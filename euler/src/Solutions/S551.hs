
module Solutions.S551 (s551, s551', s551'') where

import Debug.Trace (trace)
import Data.Map.Strict (Map, empty, update, fromList, foldlWithKey, lookup)
import Data.Char (digitToInt)

-- Find the nth number in the series formed by summing 
-- the digits of all previous numbers.

s551 :: Int -> Int
s551 n = head . take 1 . drop n $ _sieries

  where

  _sieries :: [Int]
  _sieries = _sieries' (fromList . flip zip (repeat 0) $ ['0'..'9']) 1

  _sieries' :: Map Char Int -> Int -> [Int]
  _sieries' m i =
    let m' = foldl (\m'' j -> update (return . (+(digitToInt j))) j m'') m $ show i
        i' = foldl (+) 0 m'
      in i : _sieries' m' i'


s551' :: Int -> Int
s551' n = _s551' startMap 1 0

  where

  startMap :: Map Char Int
  startMap = (fromList . flip zip (repeat 0) $ ['0'..'9'])

  _s551' :: Map Char Int -> Int -> Int -> Int
  _s551' m a i
    | i == n    = a
    | otherwise =
      let m' = foldl (\m'' j -> update (return . (+(digitToInt j))) j m'') m $ show a
          a' = foldl (+) 0 m'
        in _s551' m' a' (i + 1)



s551'' :: Int -> Int
s551'' n
  | n < 0     = 0
  | n == 0    = 1
  | otherwise = _s551'' 1 1

  where

  _s551'' :: Int -> Int -> Int
  _s551'' a i
    | i == n    = a
    | otherwise =
      let a' = foldl (+) a . fmap digitToInt . show $ a
        in if i `mod` (10 ^ 9) == 0 then trace ("a': " ++ show a' ++ ", i: " ++ show i) _s551'' a' (i + 1)
            else _s551'' a' (i + 1)


-- Solution:
-- s551 (10 ^ 16) == (?)


