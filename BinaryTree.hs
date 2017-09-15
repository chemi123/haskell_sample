data BinaryTree a = EmptyTree | Node a (BinaryTree a) (BinaryTree a) deriving (Show)

singleton :: a -> BinaryTree a
singleton x = Node x EmptyTree EmptyTree

insertTree :: (Ord a) => a -> BinaryTree a -> BinaryTree a
insertTree x EmptyTree = singleton x
insertTree x (Node a left right)
    | x == a = Node x left right
    | x < a  = Node a (insertTree x left) right
    | x > a  = Node a left (insertTree x right)

elemTree :: (Ord a) => a -> BinaryTree a -> Bool
elemTree x EmptyTree = False
elemTree x (Node a left right)
    | x == a = True
    | x < a  = elemTree x left
    | x > a  = elemTree x right
