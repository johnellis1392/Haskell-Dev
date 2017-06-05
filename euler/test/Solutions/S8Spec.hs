
module Solutions.S8Spec (spec) where

import Solutions.S8 (s8)
import Test.Hspec (Spec, describe, it, shouldBe)

spec :: Spec
spec = do
  describe "s8" $ do
    it "should get the largest product of an n-digit subsequence" $ do
      s8 4 `shouldBe` 5832
      -- s8 1 `shouldBe` 9


