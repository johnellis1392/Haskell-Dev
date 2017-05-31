module Celestia.Util.Math (fibonacci) where

-- Generate Infinite Fibonacci Sequence
fibonacci :: [Integer]
fibonacci = _fibonacci 0 1
  where
  _fibonacci n0 n1 = [n0] ++ _fibonacci n1 (n0 + n1)

