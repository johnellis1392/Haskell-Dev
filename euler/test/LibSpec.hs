
module LibSpec (spec) where

import Lib (someFunc)
import Test.Hspec (describe, it, shouldBe, Spec)

spec :: Spec
spec = do
  describe "someFunc" $ do
    it "should return 'someFunc'" $ do
      someFunc `shouldBe` "someFunc"


