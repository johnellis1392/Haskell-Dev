
module Celestia.Data.ListSpec (spec) where

import Test.Hspec (Spec, describe, context, it, shouldBe)
import Celestia.Data.List (
  duplicates,
  unique,
  diff_seq,
  slice,
  subsequences )


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


  describe "slice" $ do
    it "should get a slice from a list" $ do
      let xs = [0..100]
      slice 10 10 xs `shouldBe` [10..19]
      slice 10 1 xs `shouldBe` [10]


  describe "subsequences" $ do
    it "should calculate a list of n-digit subsequences of a given list" $ do
      let xs = [0..10]
      subsequences xs 1 `shouldBe` fmap (:[]) xs
      subsequences xs 5 `shouldBe` [[i..j - 1] | i <- [0..6], let j = i + 5]

      let ys = [0..100]
      subsequences ys 5 `shouldBe` [[i..j - 1] | i <- [0..100 - 4], let j = i + 5]



