
module Celestia.Data.ListSpec (spec) where

import Test.Hspec (Spec, describe, context, it, shouldBe)
import Celestia.Data.List (
  duplicates,
  unique,
  diff_seq )


spec :: Spec
spec = do
  describe "duplicates" $ do
    it "should calculate all duplicate elements in a list" $ do
      duplicates [1,1,2,3] `shouldBe` [1]
      duplicates [1,1,2,2,3,3] `shouldBe` [1,2,3]
      duplicates ([1..50] ++ [1..100]) `shouldBe` [1..50]
      duplicates ([1..100] ++ [1..100]) `shouldBe` [1..100]

  describe "unique" $ do
    it "should calculate all unique elements from a list" $ do
      unique [1,2,3] `shouldBe` [1,2,3]
      unique [1,1,2,2,3,3] `shouldBe` [1,2,3]
      unique ([1..50] ++ [1..100]) `shouldBe` [1..100]
      unique ([1..100] ++ [1..100]) `shouldBe` [1..100]


  describe "diff_seq" $ do
    it "should calculate a sequential list difference" $ do
      diff_seq [1,2,3] [1,2,3] `shouldBe` []
      diff_seq [1,2,3] [1] `shouldBe` [2,3]
      diff_seq [1,2,3] [3] `shouldBe` [1,2]
      diff_seq [1..100] [1..100] `shouldBe` []
      diff_seq [1..100] [2,4..100] `shouldBe` [1,3..100]


