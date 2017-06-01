module Celestia.Util.MathSpec (spec) where

import Test.Hspec (Spec, describe, context, it, shouldBe)
import Celestia.Util.Math (fibonacci, primeFactors, sieveOfEratosthenes, sieveOfSundaram, sieveOfAtkin)

spec :: Spec
spec = do
  describe "fibonacci" $ do
    it "should produce a list of fibonacci numbers" $ do
      take 10 fibonacci `shouldBe` [0, 1, 1, 2, 3, 5, 8, 13, 21, 34]
      take 21 fibonacci `shouldBe` [0, 1, 1, 2, 3, 5, 8, 13, 21, 34, 55, 89, 144, 233, 377, 610, 987, 1597, 2584, 4181, 6765]


  describe "primeFactors" $ do
    it "should generate the prime factors of a number" $ do
      primeFactors 13195 `shouldBe` [5,7,13,29]

  describe "Prime Sieves" $ do
    context "sieveOfEratosthenes" $ do
      it "should generate a list of primes" $ do
        sieveOfEratosthenes 100 `shouldBe` [2,3,5,7,11,13,17,19,23,29,31,37,41,43,47,53,59,61,67,71,73,79,83,89,97]

    context "sieveOfSundaram" $ do
      it "should generate a list of primes" $ do
        sieveOfSundaram 100 `shouldBe` [2,3,5,7,11,13,17,19,23,29,31,37,41,43,47,53,59,61,67,71,73,79,83,89,97]

    context "sieveOfAtkin" $ do
      it "should generate a list of primes" $ do
        sieveOfAtkin 100 `shouldBe` [2,3,5,7,11,13,17,19,23,29,31,37,41,43,47,53,59,61,67,71,73,79,83,89,97]



