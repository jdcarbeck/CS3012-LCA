module Spec where

import Test.Hspec
import Control.Exception (evaluate)

import Tree

{-main = hspec $ do
  describe "createTree" $ do
    it "throws an exception if used with empty list" $ do
      evaluate (createTree []) `shouldThrow` anyException
spec_Tree :: Spec
spec_Tree = do
  it "return a tree with just the element from the single element list"
    createTree (1:[]) `shouldBe` (Tree Int)

spec_Tree :: Spec
spec_Tree = do
  it "returns the given value decremented by 1"
    mathFunc 2 `shouldBe` 1

-}
main :: IO ()
main = hspec $ do
  describe "mathFunc" $ do
    it "returns the original number subtracted by 1" $
      mathFunc 1 `shouldBe` 1
