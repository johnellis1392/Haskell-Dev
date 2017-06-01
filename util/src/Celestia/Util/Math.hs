module Celestia.Util.Math (
  fibonacci,
  primeFactors,
  sieveOfEratosthenes,
  sieveOfSundaram,
  sieveOfAtkin
) where


-- Generate Infinite Fibonacci Sequence
fibonacci :: [Integer]
fibonacci = _fibonacci 0 1
  where
  _fibonacci n0 n1 = [n0] ++ _fibonacci n1 (n0 + n1)


-- Generate the prime factors of a number
primeFactors :: Int -> [Int]
primeFactors n = []


-- Prime sieves
sieveOfEratosthenes :: Int -> [Int]
sieveOfEratosthenes n = []


sieveOfSundaram :: Int -> [Int]
sieveOfSundaram n = []


sieveOfAtkin :: Int -> [Int]
sieveOfAtkin n = []



