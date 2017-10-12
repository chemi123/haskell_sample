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

class YesNo a where
    yesno :: a -> Bool


instance YesNo (BinaryTree a) where
    yesno EmptyTree = False
    yesno _ = True

instance Functor BinaryTree where
    fmap _ EmptyTree = EmptyTree
    fmap f (Node x left right)
        = Node (f x) (fmap f left) (fmap f right)

instance Foldable BinaryTree where
    foldMap f EmptyTree = mempty
    foldMap f (Node x left right) = 
        foldMap f left `mappend`
        f x            `mappend`
        foldMap f right
