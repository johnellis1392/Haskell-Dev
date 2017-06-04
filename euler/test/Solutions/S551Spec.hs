
module Solutions.S551Spec (spec) where

import Solutions.S551 (s551, s551', s551'')
import Test.Hspec (Spec, describe, it, shouldBe)

spec :: Spec
spec = do
  describe "s551" $ do
    it "should get the nth number of the digit series" $ do
      s551 0 `shouldBe` 1
      s551 1 `shouldBe` 1
      s551 2 `shouldBe` 2
      s551 3 `shouldBe` 4
      s551 4 `shouldBe` 8
      s551 5 `shouldBe` 16
      s551 6 `shouldBe` 23
      s551 7 `shouldBe` 28
      s551 8 `shouldBe` 38
      s551 9 `shouldBe` 49
      -- s551 (10 ^ 6) `shouldBe` 31054319
  describe "2551'" $ do
    it "should get the nth number of the digit series" $ do
      s551' 0 `shouldBe` 1
      s551' 1 `shouldBe` 1
      s551' 2 `shouldBe` 2
      s551' 3 `shouldBe` 4
      s551' 4 `shouldBe` 8
      s551' 5 `shouldBe` 16
      s551' 6 `shouldBe` 23
      s551' 7 `shouldBe` 28
      s551' 8 `shouldBe` 38
      s551' 9 `shouldBe` 49
      -- s551' (10 ^ 6) `shouldBe` 31054319
  describe "2551''" $ do
    it "should get the nth number of the digit series" $ do
      s551'' 0 `shouldBe` 1
      s551'' 1 `shouldBe` 1
      s551'' 2 `shouldBe` 2
      s551'' 3 `shouldBe` 4
      s551'' 4 `shouldBe` 8
      s551'' 5 `shouldBe` 16
      s551'' 6 `shouldBe` 23
      s551'' 7 `shouldBe` 28
      s551'' 8 `shouldBe` 38
      s551'' 9 `shouldBe` 49
      -- s551'' (10 ^ 6) `shouldBe` 31054319
      -- putStrLn $ show $ s551'' (10 ^ 16)



