module Solutions.S3Spec (spec) where

import Solutions.S3 (s3)
import Test.Hspec (Spec, describe, it, shouldBe)

spec :: Spec
spec = do
  describe "s3" $ do
    it "should find the largest prime factor" $ do
      s3 13195 `shouldBe` 29


