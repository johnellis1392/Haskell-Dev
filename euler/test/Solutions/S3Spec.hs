
module Solutions.S3Spec (spec) where

import Solutions.S3 (s3)
import Celestia.Util.Math (sieveOfEratosthenes)
import Test.Hspec (Spec, describe, it, shouldBe)
import Data.Foldable (for_)


spec :: Spec
spec = do
  describe "s3" $ do
    it "should find the largest prime factor" $ do
      s3 13195 `shouldBe` 29
      s3 (21 * 19) `shouldBe` 19
      s3 29 `shouldBe` 29
      s3 (2 ^ 10) `shouldBe` 2


      for_ [10..20] $ \i -> do
        let examplePrimes = take i sieveOfEratosthenes
        let expectedPrime = last examplePrimes
        let n = foldl (*) 1 examplePrimes

        s3 n `shouldBe` expectedPrime


