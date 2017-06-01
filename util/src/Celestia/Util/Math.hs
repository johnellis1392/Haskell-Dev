
module Celestia.Util.Math (
  fibonacci,
  primeFactors,
  sieveOfEratosthenes,
  sieveOfEratosthenes',
  sieveOfSundaram,
  sieveOfSundaram',
  sieveOfAtkin,
  sieveOfAtkin',
  euclidean_gcd,
  ro_factorization
) where

import Debug.Trace (trace)
import Control.Monad
import Data.List ((\\))


-- Generate Infinite Fibonacci Sequence
fibonacci :: [Integer]
fibonacci = _fibonacci 0 1
  where
  _fibonacci n0 n1 = [n0] ++ _fibonacci n1 (n0 + n1)


-- Generate the prime factors of a number
primeFactors :: Int -> [Int]
primeFactors n = _prime_factors sieveOfEratosthenes n
  where
  _prime_factors :: [Int] -> Int -> [Int]
  _prime_factors [] _ = []
  _prime_factors (head:tail) n
    | head > floor (sqrt $ fromIntegral n) = []
    | n `div` head == 0 = head : _prime_factors (head:tail) (n `div` head)
    | otherwise     = _prime_factors tail n



-- Initial seed to start prime sieves
_prime_seed :: [Int]
_prime_seed = [2]

-- Infinite list for generating values to seed prime functions
_number_generator :: [Int]
_number_generator =
  let inital_prime = last _prime_seed
      seed = if inital_prime `mod` 2 == 0 then inital_prime + 1 else inital_prime
    in [seed, seed + 2..]


-- Prime sieves
sieveOfEratosthenes :: [Int]
sieveOfEratosthenes = _prime_seed ++ _sieve _prime_seed _number_generator
  where
  _sieve :: [Int] -> [Int] -> [Int]
  _sieve primes (n:numbers)
    | any (_divisible n) primes = _sieve primes numbers
    | otherwise = n : _sieve (primes ++ [n]) numbers

  _divisible :: Int -> Int -> Bool
  _divisible i j = i `mod` j == 0

-- Sieve with integer limit
sieveOfEratosthenes' :: Int -> [Int]
sieveOfEratosthenes' n = takeWhile ((>) n) sieveOfEratosthenes


-- Sieve of Sundaram:
-- 1) For each number from 1 to n, 
--    remove all numbers in `i + j + 2ij` given:
--      i,j `in` (set of all integers), 1 <= i <= j
--      i + j + 2ij <= n
-- 2) Feed all remaining numbers through the equation:
--    f(x) = 2x + 1
sieveOfSundaram :: [Int]
sieveOfSundaram = []

sieveOfSundaram' :: Int -> [Int]
sieveOfSundaram' n = takeWhile ((>) n) _sieve
  where
  _sieve :: [Int]
  _sieve = fmap (\i -> 2 * i + 1) $ _gen \\ _primes

  _gen :: [Int]
  _gen = fmap _h . filter _f . filter _g . _make_tuples $ _primes

  _primes :: [Int]
  _primes = take n $ _prime_seed

  _make_tuples :: [Int] -> [(Int, Int)]
  _make_tuples = fmap (\[i, j] -> (i, j)) . replicateM 2

  _h :: (Int, Int) -> Int
  _h (i, j) = i + j + 2 * i * j

  _f :: (Int, Int) -> Bool
  _f (i, j) = _h (i, j) <= n

  _g :: (Int, Int) -> Bool
  _g (i, j) = i <= j


sieveOfAtkin :: [Int]
sieveOfAtkin = []

sieveOfAtkin' :: Int -> [Int]
sieveOfAtkin' n = takeWhile ((>) n) sieveOfAtkin



-- Euclidean greatest common divisor
euclidean_gcd :: Int -> Int -> Int
euclidean_gcd a b = 
  if b == 0
    then a
    else euclidean_gcd b (a `mod` b)


-- Pollard's ro prime factorization algorithm
ro_factorization :: Int -> Maybe Int
ro_factorization n = _ro_factorization n 2 2 1
  where

  _ro_factorization n' x y d
    | d == 1 =
      let x' = g x
          y' = g . g $ y
          d' = gcd (abs $ x - y) n'
        in _ro_factorization n' x' y' d'
    | d == n' = Nothing
    | otherwise = Just d

  g x' = (x' ^ 2 + 1) `mod` n

  gcd = euclidean_gcd



