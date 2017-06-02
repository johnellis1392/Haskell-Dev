module Solutions.S1Spec where

import Solutions.S1 (s1)
import Test.Hspec (Spec, describe, it, shouldBe)


spec :: Spec
spec = do
  describe "s1" $ do
    it "should produce product multiples of 3 and 5" $ do
      s1 10 `shouldBe` sum [3,5,6,9]
      s1 20 `shouldBe` sum [3,5,6,9,10,12,15,18]

