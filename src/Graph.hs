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
createGraph [] = Graph []
createGraph ((x,y):[]) = (insertEdge (x,y) (insertVertex x (insertVertex y (Graph []))))
createGraph ((x,y):z) = (insertEdge (x, y) (insertVertex x (insertVertex y (createGraph(z)))))

empty :: Graph a
empty = Graph []

isEmpty :: Eq a => Graph a -> Bool
isEmpty a | a == empty = True
          | otherwise = False

isEqual :: Eq a => Graph a -> Graph a -> Bool
isEqual x y | x == y = True
            | otherwise = False

insertVertex :: Eq a => a -> Graph a -> Graph a
insertVertex x graph | (checkInGraph x graph) = graph
insertVertex x (Graph []) = (Graph ((x,[]):[]))
insertVertex x (Graph ((a,bs):[])) = (Graph ((a,bs):(x,[]):[]))
insertVertex x (Graph((a,bs):c)) = Graph(((a,bs):c) ++ ((x,[]):[]))

checkInGraph :: Eq a => a -> Graph a -> Bool
checkInGraph x (Graph []) = False
checkInGraph x (Graph ((a,_):[]))
                    | x == a = True
                    | otherwise = False
checkInGraph x (Graph ((a,_):b))
                    | x == a = True
                    | otherwise = checkInGraph x (Graph (b))

insertEdge :: Eq a => (a,a) -> Graph a -> Graph a
insertEdge (x,y) (Graph []) = (Graph [])
insertEdge (x,y) (Graph ((a,bs):[]))
                      | x == a = (Graph ((a,(addAdj y bs)):[]))
                      | otherwise = (Graph ((a,bs):[]))
insertEdge (x,y) (Graph((a,bs):c))
          | x == a = (Graph((a,(addAdj y bs)):c))
          | otherwise = (joinGraph (Graph((a,bs):[])) (insertEdge (x,y) (Graph(c))))

addAdj :: Eq a => a -> [a] -> [a]
addAdj a [] = [a]
addAdj a (x:xs) | a == x = [a]
                | otherwise = (x:(addAdj a xs))

joinGraph :: Eq a => Graph a -> Graph a -> Graph a
joinGraph (Graph []) (Graph y) = (Graph y)
joinGraph (Graph x) (Graph []) = (Graph x)
joinGraph (Graph x) (Graph y) = (Graph(x++y))

lca :: Eq a => (a,a) -> Graph a -> [a]
lca (x,y) (Graph []) = []
-- lca = undefined
