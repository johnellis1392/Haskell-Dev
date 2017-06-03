
module Solutions.S4Spec (spec) where

import Solutions.S4 (s4)
import Test.Hspec (Spec, describe, it, shouldBe)

spec :: Spec
spec = do
  describe "s4" $ do
    it "should calculate a palindrome product" $ do
      s4 1 `shouldBe` 9
      s4 2 `shouldBe` 9009
      s4 3 `shouldBe` 906609


