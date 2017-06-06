
module Solutions.S10Spec (spec) where

import Solutions.S10 (s10)
import Test.Hspec (Spec, describe, it, shouldBe)


spec :: Spec
spec = do
  describe "s10" $ do
    it "should calculate the sum of n primes" $ do
      s10 10 `shouldBe` sum [2,3,5,7]
      s10 20 `shouldBe` sum [2,3,5,7,11,13,17,19]



