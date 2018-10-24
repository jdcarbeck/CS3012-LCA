{-# LANGUAGE StandaloneDeriving #-}
module Main where

import Test.HUnit
import Test.Framework as TF (defaultMain, testGroup, Test)
import Test.Framework.Providers.HUnit (testCase)

import Tree

main = defaultMain tests --run testCase

testTree :: Tree Int
testTree = Node 4
  (Node 2
    (Node 1 Empty Empty)
    (Node 3 Empty Empty))
  (Node 6
    (Node 5 Empty Empty)
    (Node 7 Empty Empty))

tests :: [TF.Test]
tests = [testGroup "\n\nTesting for LCA"
          [ creationOfTreeFromList
          , pathFromGivenTree
          , commonPathInGivenTree
          ]
        ]

creationOfTreeFromList :: TF.Test
creationOfTreeFromList
 = testGroup "\nTree creation from a list"
    [ testCase "Creates a tree with 1 element list"
        ( lca 1 1 (createTree [1]) @?= 1 )
    , testCase "Creates a tree with 2 element list"
        ( lca 1 2 (createTree [1,2]) @?= 2 )
    , testCase "Creates a tree with 3 element list"
       ( lca 1 3 (createTree [1,3,2]) @?= 2 )
    ]

pathFromGivenTree :: TF.Test
pathFromGivenTree
 = testGroup "\nPaths of node given in the sample balenced tree"
    [ testCase "Path of Node 1"
        ( findPath 1 testTree @?= [4,2,1] )
    , testCase "Path of Node 2"
        ( findPath 2 testTree @?= [4,2] )
    , testCase "Path of Node 3"
        ( findPath 3 testTree @?= [4,2,3] )
    , testCase "Path of Node 4"
        ( findPath 4 testTree @?= [4] )
    , testCase "Path of Node 5"
        ( findPath 5 testTree @?= [4,6,5] )
    , testCase "Path of Node 6"
        ( findPath 6 testTree @?= [4,6] )
    , testCase "Path of Node 7"
        ( findPath 7 testTree @?= [4,6,7] )
    ]

commonPathInGivenTree :: TF.Test
commonPathInGivenTree
 = testGroup "\nCommon paths of given nodes in sample balenced tree"
    [ testCase "CommonPath of elems without a common path"
        ( commonPath [1,4,5] [] @?= [] )
    , testCase "CommonPath of two leaf nodes"
        ( commonPath (findPath 7 testTree) (findPath 3 testTree) @?= [4] )
    , testCase "CommonPath of child and parent"
        ( commonPath (findPath 2 testTree) (findPath 1 testTree) @?= [4,2] )
    , testCase "CommonPath of two child node with same parent"
        ( commonPath (findPath 3 testTree) (findPath 1 testTree) @?= [4,2] )
    ]

    -- describe "lca from testTree in Spec.hs" $ do
    --   it "returns correct lca for 1 2 testTree" $
    --     lca 1 2 testTree `shouldBe` 2
    --   it "returns correct lca for 1 3 testTree" $
    --     lca 1 3 testTree `shouldBe` 2
    --   it "returns correct lca for 1 4 testTree" $
    --     lca 1 4 testTree `shouldBe` 4
    --   it "returns correct lca for 1 5 testTree" $
    --     lca 1 5 testTree `shouldBe` 4
    --   it "returns correct lca for 1 6 testTree" $
    --     lca 1 6 testTree `shouldBe` 4
    --   it "returns correct lca for 1 7 testTree" $
    --     lca 1 7 testTree `shouldBe` 4
    --   it "returns correct lca for 2 3 testTree" $
    --     lca 2 3 testTree `shouldBe` 2
    --   it "returns correct lca for 2 4 testTree" $
    --     lca 2 4 testTree `shouldBe` 4
    --   it "returns correct lca for 2 5 testTree" $
    --     lca 2 5 testTree `shouldBe` 4
    --   it "returns correct lca for 2 6 testTree" $
    --     lca 2 6 testTree `shouldBe` 4
    --   it "returns correct lca for 2 7 testTree" $
    --     lca 2 7 testTree `shouldBe` 4
    --   it "returns correct lca for 3 4 testTree" $
    --     lca 3 4 testTree `shouldBe` 4
    --   it "returns correct lca for 3 5 testTree" $
    --     lca 3 5 testTree `shouldBe` 4
    --   it "returns correct lca for 3 6 testTree" $
    --     lca 3 6 testTree `shouldBe` 4
    --   it "returns correct lca for 3 7 testTree" $
    --     lca 3 7 testTree `shouldBe` 4
    --   it "returns correct lca for 4 5 testTree" $
    --     lca 4 5 testTree `shouldBe` 4
    --   it "returns correct lca for 4 6 testTree" $
    --     lca 4 6 testTree `shouldBe` 4
    --   it "returns correct lca for 4 7 testTree" $
    --     lca 4 7 testTree `shouldBe` 4
    --   it "returns correct lca for 5 6 testTree" $
    --     lca 5 6 testTree `shouldBe` 6
    --   it "returns correct lca for 5 7 testTree" $
    --     lca 5 7 testTree `shouldBe` 6
    --   it "returns correct lca for 6 7 testTree" $
    --     lca 6 7 testTree `shouldBe` 6
