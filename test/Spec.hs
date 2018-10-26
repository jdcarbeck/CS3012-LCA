{-# LANGUAGE StandaloneDeriving #-}
module Main where

import Test.HUnit
import Test.Framework as TF (defaultMain, testGroup, Test)
import Test.Framework.Providers.HUnit (testCase)

import Graph

main = defaultMain tests --run testCase

emptyGraph = Graph [] :: Graph Int
nonEmptyGraph = Graph [(1,[])] :: Graph Int
exampleGraph = Graph [(1,[]), (2,[1])] :: Graph Int

tests :: [TF.Test]
tests = [testGroup "\n\nTesting for LCA"
          [ checkingEmptinessOfGraph
          , checkingEquivalenceOfGraph
          , insertingVertexIntoGraph
          -- , insertingEdgeIntoGraph
          -- , creationOfGraphFromList
          -- , bfsForGivenVertex
          -- , lcaForGivenVertices
          ]
        ]

checkingEmptinessOfGraph :: TF.Test
checkingEmptinessOfGraph
 = testGroup "\nChecking if a graph is empty or not"
    [ testCase "Check a non-empty graph to be empty"
       ( isEmpty nonEmptyGraph @?= False )
    , testCase "Check a empty graph to be empty"
        ( isEmpty emptyGraph @?= True )
    ]

checkingEquivalenceOfGraph :: TF.Test
checkingEquivalenceOfGraph
 = testGroup "\nChecking if two graphs are equivalent"
   [ testCase "Check two empty graphs to be equal"
        ( isEqual emptyGraph emptyGraph @?= True )
   , testCase "Check two nonempty equal graphs to be equivalent"
        ( isEqual nonEmptyGraph nonEmptyGraph @?= True )
   , testCase "Check two nonempty unequal graphs to be equivalent"
        ( isEqual nonEmptyGraph exampleGraph @?= False )
   ]

insertingVertexIntoGraph :: TF.Test
insertingVertexIntoGraph
 = testGroup "\nInserting a vertex into an existing graph"
 [ testCase "Check the insertion of an element of into an nonempty graph"
        ( isEqual (Graph [(1,[]),(2,[])] :: Graph Int)
                  (insertVertex 2 nonEmptyGraph) @?= True )
 , testCase "Check the insertion of an element into an empty graph"
        ( isEqual (Graph [(1,[])])(insertVertex 1 emptyGraph) @?= True )
 , testCase "Check the insertion of an dupulicate element into an graph"
        ( isEqual nonEmptyGraph (insertVertex 1 nonEmptyGraph) @?= True )
 ]


-- creationOfGraphFromList :: TF.Test
-- creationOfGraphFromList
--  = testGroup "\nChecking if a graph can be created from lists of elements"
--    [ testCase "Check the creation of a graph from empty list"
--         -- ( isEmpty emptyGraph @=? True )
--   --  , testCase "Check the creation of a graph from a single element list"
--   --  , testCase "Check the creation of a graph from a multi element list"
--    ]
