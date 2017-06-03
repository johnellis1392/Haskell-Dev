
module Solutions.S5Spec (spec) where

import Solutions.S5 (s5)
import Test.Hspec (Spec, describe, it, shouldBe)

spec :: Spec
spec = do
  describe "s5" $ do
    it "should calculate the least divisible number" $ do
      s5 3 `shouldBe` 6
      s5 4 `shouldBe` 12
      s5 10 `shouldBe` 2520


