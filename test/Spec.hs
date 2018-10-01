module Spec where
import Test.Hspec
import Test.QuickCheck
import Control.Exception (evaluate)
import Tree

{-main = hspec $ do
  describe "createTree" $ do
    it "throws an exception if used with empty list" $ do
      evaluate (createTree []) `shouldThrow` anyException
-}
spec_Tree :: Spec
spec_Tree = do
  it "return a tree with just the element from the single element list"
    createTree (1:[]) `shouldBe` (Tree Int)
