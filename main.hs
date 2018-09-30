import Text.Printf
data BinaryTree a = Node { value :: a,
                           left :: (BinaryTree a),
                           right :: (BinaryTree a)}
                  | Leaf deriving Show
tree :: BinaryTree Int
tree = Node 1
  (Node 2
    (Node 4 Leaf Leaf)
    (Node 5 Leaf Leaf))
  (Node 3
    (Node 6 Leaf Leaf)
    (Node 7 Leaf Leaf))
