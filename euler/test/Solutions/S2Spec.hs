
module Solutions.S2Spec (spec) where

import Solutions.S2 (s2)
import Test.Hspec (Spec, describe, it, shouldBe)

spec :: Spec
spec = do
  describe "s2" $ do
    it "should produce the sum of the first 'n' digits of the fibonacci sequence" $ do
      s2 10 `shouldBe` sum [2,8,34]
      s2 20 `shouldBe` sum [2,8,34,144,610,2584,10946]

