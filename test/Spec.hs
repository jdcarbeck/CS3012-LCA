{-# LANGUAGE StandaloneDeriving #-}
module Main where

import Test.HUnit
import Test.Framework as TF (defaultMain, testGroup, Test)
import Test.Framework.Providers.HUnit (testCase)

import Graph

main = defaultMain tests --run testCase

emptyGraph = Graph [] :: Graph Int
nonEmptyGraph = Graph [(1,[])] :: Graph Int
nonEmptyGraph1 = Graph [(2,[])] :: Graph Int
exampleGraph = Graph [(1,[]), (2,[1])] :: Graph Int
exampleGraph1 = Graph [(1,[]), (3,[1])] :: Graph Int
exampleGraph2 = Graph [(1,[]), (3,[])] :: Graph Int

tests :: [TF.Test]
tests = [testGroup "\n\nTesting for LCA"
          [ checkingEmptinessOfGraph
          , checkingEquivalenceOfGraph
          , checkingVertexInGraph
          , insertingVertexIntoGraph
          , insertingEdgeIntoGraph
          -- , creationOfGraphFromList
          -- , bfsForGivenVertex
          -- , lcaForGivenVertices
          ]
        ]

checkingEmptinessOfGraph :: TF.Test
checkingEmptinessOfGraph
 = testGroup "\nChecking if a graph is empty or not"
    [ testCase "Check a non-empty graph"
       ( isEmpty nonEmptyGraph @?= False )
    , testCase "Check a empty graph"
        ( isEmpty emptyGraph @?= True )
    ]

checkingEquivalenceOfGraph :: TF.Test
checkingEquivalenceOfGraph
 = testGroup "\nChecking if two graphs are equivalent"
    [ testCase "Check two empty graphs to be equal"
        ( isEqual emptyGraph emptyGraph @?= True )
    , testCase "Check two 1 elem nonempty equal graphs"
        ( isEqual nonEmptyGraph nonEmptyGraph @?= True )
    , testCase "Check two 1 elem nonempty unequal"
        ( isEqual nonEmptyGraph nonEmptyGraph1 @?= False )
    , testCase "Check two nonempty equal graphs"
        ( isEqual exampleGraph  exampleGraph @?= True )
    , testCase "Check two nonempty unequal graphs"
        ( isEqual exampleGraph1 exampleGraph @?= False)
    , testCase "Check two different sized graphs"
        ( isEqual exampleGraph nonEmptyGraph @?= False)
    , testCase "Check two graphs with same vertices but different edges"
        ( isEqual exampleGraph1 exampleGraph2 @?= False)
    ]

checkingVertexInGraph :: TF.Test
checkingVertexInGraph
 = testGroup "\nChecking if an vertex is within a graph"
   [ testCase "Check if vertex is in a empty graph"
        ( checkInGraph 1 emptyGraph @?= False )
   , testCase "Check if vertex is in a 1 elem graph containing the given vertex"
        ( checkInGraph 1 nonEmptyGraph @?= True )
   , testCase "Check if vertex is in a 1 elem graph not containg the given vertex"
        ( checkInGraph 2 nonEmptyGraph @?= False )
   , testCase "Check if vertex is in a graph containing the given vertex"
        ( checkInGraph 2 exampleGraph @?= True )
   , testCase "Check if vertex is in a graph not containg the given vertex"
        ( checkInGraph 3 exampleGraph @?= False)
   ]

testGraph = Graph [(1,[]), (3,[1]), (2,[1]), (4,[2,3])] :: Graph Int
testGraph1 = Graph [(1,[]), (3,[1]), (2,[1]), (4,[2,3]), (5,[])] :: Graph Int

insertingVertexIntoGraph :: TF.Test
insertingVertexIntoGraph
 = testGroup "\nInserting a vertex into an existing graph"
  [ testCase "Check the insertion of an vertex into an empty graph"
          ( isEqual (Graph [(1,[])])(insertVertex 1 emptyGraph) @?= True )
   , testCase "Check the insertion of an duplicate vertex into an 1 elem graph"
          ( isEqual nonEmptyGraph (insertVertex 1 nonEmptyGraph) @?= True )
   , testCase "Check the insertion of an vertex of into an nonempty 1 elem graph"
          ( isEqual (Graph [(1,[]),(2,[])] :: Graph Int)
                    (insertVertex 2 nonEmptyGraph) @?= True )
   , testCase "Check the insertion of an duplicate vertex into a graph"
          ( isEqual (testGraph) (insertVertex 4 testGraph) @?= True )
   , testCase "Check the insertion of an vertex into a graph"
          ( isEqual (testGraph1) (insertVertex 5 testGraph) @?= True )
  ]

testEdge = Graph [(1,[]), (2,[1])] ::Graph Int
testEdge1 = Graph [(1,[]), (3,[1]), (2,[1])] :: Graph Int
testEdge2 = Graph [(1,[]), (3,[1]), (2,[1]), (4,[3])] :: Graph Int
testEdge3 = Graph [(1,[]), (3,[1]), (2,[1]), (4,[3,2])] :: Graph Int
testEdge4 = Graph [(1,[]), (3,[1]), (2,[1,5]), (4,[3,2]), (5,[])] :: Graph Int

insertingEdgeIntoGraph :: TF.Test
insertingEdgeIntoGraph
 = testGroup "\nInserting a edge into an existing graph"
  [ testCase "Check the insertion of an edge into an empty graph"
        ( isEqual (testEdge) (insertEdge (1,2) emptyGraph) @?= True )
  , testCase "Check the insertion of an edge into a graph with exsiting verticies"
        ( isEqual (testEdge3) (insertEdge (2,4) testEdge2) @?= True )
  , testCase "Check the insertion of an edge into a graph with just the parent"
        ( isEqual (testEdge1) (insertEdge (3,4) testEdge1) @?= True )
  , testCase "Check the insertion of an edge into a graph with just the child"
        ( isEqual (testEdge4) (insertEdge (5,2) testEdge3) @?= True )
  ]

-- creationOfGraphFromList :: TF.Test
-- creationOfGraphFromList
--  = testGroup "\nChecking if a graph can be created from lists of elements"
--    [ testCase "Check the creation of a graph from empty list"
--         -- ( isEmpty emptyGraph @=? True )
--   --  , testCase "Check the creation of a graph from a single element list"
--   --  , testCase "Check the creation of a graph from a multi element list"
--    ]
