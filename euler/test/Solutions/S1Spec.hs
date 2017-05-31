
module Solutions.S1Spec where

import Solutions.S1 (main)
import Test.Hspec (Spec, describe, it, shouldBe)


spec :: Spec
spec = do
  describe "main" $ do
    it "should produce product multiples of 3 and 5" $ do
      main 10 `shouldBe` product [3,5,6,9,10]
      main 20 `shouldBe` product [3,5,6,9,10,12,15,18,20]

