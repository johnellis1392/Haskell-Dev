module Celestia.Util.MathSpec (spec) where

import Test.Hspec (Spec, describe, it, shouldBe)
import Celestia.Util.Math (fibonacci)

spec :: Spec
spec = do
  describe "fibonacci" $ do
    it "should produce a list of fibonacci numbers" $ do
      take 10 fibonacci `shouldBe` [0, 1, 1, 2, 3, 5, 8, 13, 21, 34]
      take 21 fibonacci `shouldBe` [0, 1, 1, 2, 3, 5, 8, 13, 21, 34, 55, 89, 144, 233, 377, 610, 987, 1597, 2584, 4181, 6765]


