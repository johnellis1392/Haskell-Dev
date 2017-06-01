module Celestia (
  someFunc,
  euclidean_gcd,
  ro_factorization
) where


someFunc :: String
someFunc = "someFunc"


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
  _ro_factorization :: Int -> Int -> Int -> Int -> Maybe Int
  _ro_factorization n' x y d =
    if d == 1 
    then let
        x' = g x
        y' = g . g $ y
        d' = gcd (abs $ x - y) n'
      in _ro_factorization n' x' y' d'
    else if d == n
      then Nothing
      else Just d

  g :: Int -> Int
  g x' = (x' * x' + 1) `mod` n

  gcd :: Int -> Int -> Int
  gcd = euclidean_gcd


