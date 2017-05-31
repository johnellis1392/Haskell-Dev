
module UtilSpec (spec) where

import Test.Hspec (describe, it, shouldBe, Spec)
import Util (hello)

spec :: Spec
spec = do
  describe "hello" $ do
    it "should return 'Hello'" $ do
      hello `shouldBe` "Hello"

