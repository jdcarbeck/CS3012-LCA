import Text.Printf
data Tree a = Empty
            | Node a (Tree a) (Tree a) deriving (Show)

--Takes a binary Tree and returns the value to the left of it
tree :: Tree Int
tree = Node 1
  (Node 2
    (Node 4 Empty Empty)
    (Node 5 Empty Empty))
  (Node 3
    (Node 6 Empty Empty)
    (Node 7 Empty Empty))

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

--Create a tree from a file of csv of Ints

