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
