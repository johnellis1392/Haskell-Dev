
module UtilSpec (spec) where

import Test.Hspec (describe, it, shouldBe, Spec)
import Util (hello, goodbye, someFunc2)

spec :: Spec
spec = do
  describe "hello" $ do
    it "should return 'Hello'" $ do
      hello `shouldBe` "Hello"

  describe "goodbye" $ do
    it "should return 'Goodbye'" $ do
      goodbye `shouldBe` "Goodbye"

  describe "someFunc2" $ do
    it "should return 'someFunc'" $ do
      someFunc2 `shouldBe` "someFunc"

