module Spec where

import Test.Hspec
import Control.Exception (evaluate)

import Tree

main :: IO ()
main = hspec $ do
  describe "lca" $ do
    it "returns the lca for a tree with 2 elements" $
      lca 1 2 (createTree [1,2]) `shouldBe` 2
    it "returns the lca for a tree with 3 elements" $
      lca 1 3 (createTree [1,3,2]) `shouldBe` 2
