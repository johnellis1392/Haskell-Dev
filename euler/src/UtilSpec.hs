
module UtilSpec (spec) where

import Test.Hspec (describe, it, shouldBe, Spec)
import Util (hello, goodbye)

spec :: Spec
spec = do
  describe "hello" $ do
    it "should return 'Hello'" $ do
      hello `shouldBe` "Hello"

  describe "goodbye" $ do
    it "should return 'Goodbye'" $ do
      goodbye `shouldBe` "Goodbye"

