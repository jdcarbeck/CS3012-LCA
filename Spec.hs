module Spec where

import Test.Hspec
import Control.Exception (evaluate)

import Tree

testTree :: Tree Int
testTree = Node 4
  (Node 2
    (Node 1 Empty Empty)
    (Node 3 Empty Empty))
  (Node 6
    (Node 5 Empty Empty)
    (Node 7 Empty Empty))

main :: IO ()
main = hspec $ do
  describe "lca from creation from list" $ do
    it "returns the lca for a tree created from a list with 1 element" $
      lca 1 1 (createTree [1]) `shouldBe` 1
    it "returns the lca for a tree created from a list with 2 elements" $
      lca 1 2 (createTree [1,2]) `shouldBe` 2
    it "returns the lca for a tree created from a list with 3 elements" $
      lca 1 3 (createTree [1,3,2]) `shouldBe` 2
  describe "lca from testTree in Spec.hs" $ do
    it "returns correct lca for 1 2 testTree" $
      lca 1 2 testTree `shouldBe` 2
    it "returns correct lca for 1 3 testTree" $
      lca 1 3 testTree `shouldBe` 2
    it "returns correct lca for 1 4 testTree" $
      lca 1 4 testTree `shouldBe` 4
    it "returns correct lca for 1 5 testTree" $
      lca 1 5 testTree `shouldBe` 4
    it "returns correct lca for 1 6 testTree" $
      lca 1 6 testTree `shouldBe` 4
    it "returns correct lca for 1 7 testTree" $
      lca 1 7 testTree `shouldBe` 4
    it "returns correct lca for 2 3 testTree" $
      lca 2 3 testTree `shouldBe` 2
    it "returns correct lca for 2 4 testTree" $
      lca 2 4 testTree `shouldBe` 4
    it "returns correct lca for 2 5 testTree" $
      lca 2 5 testTree `shouldBe` 4
    it "returns correct lca for 2 6 testTree" $
      lca 2 6 testTree `shouldBe` 4
    it "returns correct lca for 2 7 testTree" $
      lca 2 7 testTree `shouldBe` 4
    it "returns correct lca for 3 4 testTree" $
      lca 3 4 testTree `shouldBe` 4
    it "returns correct lca for 3 5 testTree" $
      lca 3 5 testTree `shouldBe` 4
    it "returns correct lca for 3 6 testTree" $
      lca 3 6 testTree `shouldBe` 4
    it "returns correct lca for 3 7 testTree" $
      lca 3 7 testTree `shouldBe` 4
    it "returns correct lca for 4 5 testTree" $
      lca 4 5 testTree `shouldBe` 4
    it "returns correct lca for 4 6 testTree" $
      lca 4 6 testTree `shouldBe` 4
    it "returns correct lca for 4 7 testTree" $
      lca 4 7 testTree `shouldBe` 4
    it "returns correct lca for 5 6 testTree" $
      lca 5 6 testTree `shouldBe` 6
    it "returns correct lca for 5 7 testTree" $
      lca 5 7 testTree `shouldBe` 6
    it "returns correct lca for 6 7 testTree" $
      lca 6 7 testTree `shouldBe` 6
