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

--Data types to navigate trees by going left and right
data Direction = L | R deriving (Show)
type Directions = [Direction]

changeTop :: Directions -> Tree Int -> Tree Int
changeTop (L:ds) (Node x l r) = Node x (changeTop ds l) r
changeTop (R:ds) (Node x l r) = Node x l (changeTop ds r)
changeTop [] (Node _ l r) = Node 9 l r

--Takes a int and adds it two a tree so that is sorted
addToTree :: Int -> Tree Int -> Tree Int
addToTree x Empty = (Node x Empty Empty)
addToTree x (Node i treeL treeR)
            | x <= i = Node i (addToTree x treeL) treeR
            | otherwise = Node i treeL (addToTree x treeR)

--Takes a list of Ints and returns an ordered Tree
createTree :: [Int] -> Tree Int
createTree [] = error "cannot create tree from empty list"
createTree (x:[]) = (Node x Empty Empty)
createTree (x:xs) = addToTree x (createTree xs)
