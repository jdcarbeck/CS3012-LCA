module Graph where

-- TODO: Assume that you want to find the ancestors of x and y in a graph.
--
-- Maintain an array of vectors- parents (storing parents of each node).
--
-- Firstly do a bfs(keep storing parents of each vertex) and find all the ancestors of x (find parents of x and using parents, find all the ancestors of x) and store them in a vector. Also, store the depth of each parent in the vector.
-- Find the ancestors of y using same method and store them in another vector. Now, you have two vectors storing the ancestors of x and y respectively along with their depth.
-- LCA would be common ancestor with greatest depth. Depth is defined as longest distance from root(vertex with in_degree=0). Now, we can sort the vectors in decreasing order of their depths and find out the LCA. Using this method, we can even find multiple LCA's (if there).

data Graph a = Graph [(a, [a])] deriving (Eq, Show)

createGraph ::Eq a => [(a,a)] -> Graph a
createGraph = undefined

empty :: Graph a
empty = Graph []

isEmpty :: Eq a => Graph a -> Bool
isEmpty a | a == empty = True
          | otherwise = False

isEqual :: Eq a => Graph a -> Graph a -> Bool
isEqual x y | x == y = True
            | otherwise = False

-- insert if not already in the Graph (with empty edges)
insertVertex :: Eq a => a -> Graph a -> Graph a
insertVertex x graph = undefined
-- insertVertex x [] = Graph [(x, [])]

insertEdge :: Eq a => (a,a) -> Graph a -> Graph a
insertEdge = undefined -- insert edge in list of origin

bfs :: Eq a => a -> Graph a -> [a]
bfs = undefined

lca :: Eq a => (a,a) -> Graph a -> a
lca = undefined
