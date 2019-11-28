
data Tree a = Empty | Node a (Tree a) (Tree a) deriving (Eq, Show)

reflect :: Tree a -> Tree a
reflect Empty = Empty
reflect (Node a l r) = Node a (reflect r) (reflect l)
