
module Solutions.S7Spec (spec) where

import Solutions.S7 (s7)
import Celestia.Util.Math (sieveOfEratosthenes)
import Test.Hspec (Spec, describe, it, shouldBe)

spec :: Spec
spec = do
  describe "s7" $ do
    it "should get the nth prime number" $ do
      s7 6 `shouldBe` 13
      s7 10 `shouldBe` (head . take 1 . drop 9 $ sieveOfEratosthenes)

