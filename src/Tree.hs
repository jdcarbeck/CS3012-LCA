module Tree where

data Tree a = Empty
            | Node a (Tree a) (Tree a) deriving (Show)

--Takes a list of Ints and returns an ordered Tree
createTree :: [Int] -> Tree Int
createTree [] = error "cannot create tree from empty list"
createTree (x:[]) = (Node x Empty Empty)
createTree (x:xs) = addToTree x (createTree xs)

--Takes a int and adds it two a tree so that is sorted
addToTree :: Int -> Tree Int -> Tree Int
addToTree x Empty = (Node x Empty Empty)
addToTree x (Node i treeL treeR)
          | x <= i = Node i (addToTree x treeL) treeR
          | otherwise = Node i treeL (addToTree x treeR)

--Preforms LCA on two given values
lca :: Int -> Int -> Tree Int -> Int
lca x y tree = last (commonPath(findPath x tree)(findPath y tree))

--Finds path for a given int in tree
findPath :: Int -> Tree Int -> [Int]
findPath x Empty = error "value not in the given tree"
findPath x (Node i treeL treeR)
          | x == i = i:[]
          | x < i = i:(findPath x treeL)
          | otherwise = i:(findPath x treeR)

--Finds common path between two paths
commonPath :: [Int] -> [Int] -> [Int]
commonPath (x:xs) [] = []
commonPath [] (y:ys) = []
commonPath (x:[]) (y:[])
          | x == y = x:[]
          | otherwise = []
commonPath (x:xs) (y:ys)
          | x == y = x:(commonPath xs ys)
