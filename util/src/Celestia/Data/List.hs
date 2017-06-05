
module Celestia.Data.List (
  duplicates,
  unique,
  diff_seq,
  slice,
  subsequences
) where

import Debug.Trace (traceShowId)
import Control.Monad (replicateM)
import Data.Traversable (forM)
import Data.List (
  reverse,
  intersperse,
  intercalate,
  transpose,
  -- subsequences,
  permutations,
  scanl,
  mapAccumL, -- :: Traversable t => (a -> b -> (a, c)) -> a -> t b -> (a, t c)
  iterate, -- iterate f x == [x, f x, f (f x), ...]
  repeat, -- Like replicate but unbounded
  replicate,
  cycle, -- Turns finite list into infinitely-repeating list
  unfoldr ) -- :: (b -> Maybe (a, b)) -> b -> [a]


-- Get a list of duplicates from a list (lazy)
duplicates :: Eq a => [a] -> [a]
duplicates xs = duplicates' xs []
  where
  duplicates' [] _ = []
  duplicates' (x:xs) visited
    | x `elem` visited = x : duplicates' xs visited
    | otherwise        = duplicates' xs (x:visited)


-- Get only unique elements from list (lazy)
unique :: Eq a => [a] -> [a]
unique xs = unique' xs []
  where
  unique' [] _ = []
  unique' (x:xs) visited
    | x `elem` visited = unique' xs visited
    | otherwise        = x : unique' xs (x:visited)



-- Calculate a sequential list difference between two ordered elements (lazy)
-- NOTE: Expects both lists to be ordered
diff_seq :: Eq a => [a] -> [a] -> [a]
diff_seq [] _ = []
diff_seq x [] = x
diff_seq (x:xs) (y:ys)
  | x == y    = diff_seq xs ys
  | otherwise = x : diff_seq xs (y:ys)



-- Get an n-element slice from a list
slice :: Int -> Int -> [a] -> [a]
slice start num xs = take num . drop start $ xs


-- Calculate a list of subsequences from a given list
subsequences :: [a] -> Int -> [[a]]
subsequences xs n = fmap (\i -> slice i n xs) [0..length xs - n]



