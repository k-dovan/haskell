
data Tree a = Node a [Tree a] deriving (Eq, Show)

depth_of_tree :: Tree a -> Int
depth_of_tree (Node a []) = 0
depth_of_tree (Node a (x:xs)) = max (1 + depth_of_tree x) (depth_of_tree (Node a xs))