
module Solutions.S6 (s6) where

import Control.Arrow ((***))
import Control.Monad (join)
import Data.Bifunctor (bimap, first)

-- Calculate the sum of squares of n - square of sum of n

s6 :: Int -> Int
s6 n = uncurry (-) . first (^2) $ foldl (\i j -> (+j) *** (+(j^2)) $ i) (0,0) [1..n]

  where

  -- The (***) operator maps over two functions, lifting
  -- them into Arrows, and making a functional bimap that
  -- gets applied to its arguments
  mapTuple :: (a -> a) -> (a, a) -> (a, a)
  mapTuple = join (***)

  -- Join takes a function that accepts two functions as
  -- arguments, and applies one function as both arguments.
  --
  -- Bimap maps over a Bifunctor, which is a 2-element
  -- functor.
  mapTuple' :: (a -> a) -> (a, a) -> (a, a)
  mapTuple' = join bimap


-- Solution:
-- s6 100 == 25164150

