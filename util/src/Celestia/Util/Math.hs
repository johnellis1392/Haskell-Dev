
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


-- Prime sieves
-- sieveOfEratosthenes :: Int -> [Int]
-- sieveOfEratosthenes n = []
sieveOfEratosthenes :: [Int]
sieveOfEratosthenes = _prime_seed ++ _sieve _prime_seed [(last _prime_seed)..]
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


sieveOfSundaram :: [Int]
sieveOfSundaram = []

sieveOfSundaram' :: Int -> [Int]
sieveOfSundaram' n = takeWhile ((>) n) sieveOfSundaram


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



