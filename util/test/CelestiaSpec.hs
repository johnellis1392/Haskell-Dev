
module CelestiaSpec (spec) where
import Celestia (someFunc, euclidean_gcd, ro_factorization)
import Test.Hspec (describe, it, shouldBe, Spec)

spec :: Spec
spec = do
  describe "someFunc" $ do
    it "should return 'someFunc'" $ do
      someFunc `shouldBe` "someFunc"

  describe "euclidean_gcd" $ do
    it "should calculate the greatest common divisor" $ do
      euclidean_gcd 1071 462 `shouldBe` 21

  describe "ro_factorization" $ do
    it "should generate a complete prime factorization for a number" $ do
      ro_factorization 91 `shouldBe` Just 7
      -- ro_factorization 91 `shouldBe` Just [7,13]


