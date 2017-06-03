
module Solutions.S6Spec (spec) where

import Solutions.S6 (s6)
import Test.Hspec (Spec, describe, it, shouldBe)

spec :: Spec
spec = do
  describe "s6" $ do
    it "should calculate the sosq - sqos" $ do
      s6 10 `shouldBe` 2640
      

