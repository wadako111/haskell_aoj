data Tree a = EmptyTree | Node a (Tree a) (Tree a) deriving (Show)
singleton :: a -> Tree a
singleton x = Node x EmptyTree EmptyTree

treeInsert :: (Ord a) => a -> Tree a -> Tree a
treeInsert item EmptyTree = singleton item
treeInsert item (Node a left right )
    | item == a = Node item left right
    | item < a  = Node a (treeInsert item left) right
    | item > a  = Node a left (treeInsert item right)

treeElem :: (Ord a) => a -> Tree a -> Bool
treeElem item EmptyTree = False
treeElem item (Node a left right)
  | item == a = True
  | item < a  = treeElem item left
  | item > a  = treeElem item right

instance Functor Tree where
  fmap f (Node a left right) = Node (f a) (fmap f left) (fmap f right)
  fmap f EmptyTree = EmptyTree
