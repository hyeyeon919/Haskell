rex :: [a] -> [a]
rex x = h_rex x []

h_rex :: [a] -> [a] -> [a]
h_rex [] ans = ans
h_rex (x:xs) ans = h_rex xs (x:ans)

data Tree a = Empty | Node (Tree a) a (Tree a) deriving (Show)

-- inserts an element into a bst
insertT :: Ord a => a -> Tree a -> Tree a
insertT a Empty = Node Empty a Empty
insertT a (Node left val right)
    | a < val   = Node (insertT a left) val right
    | a > val   = Node left val (insertT a right)
    | otherwise = Node left a right

-- finds an element in a bst
findT :: Ord a => a -> Tree a -> Maybe a
findT _ Empty = Nothing
findT a (Node left val right)
    | a < val   = findT a left
    | a > val   = findT a right
    | otherwise = Just a

-- mirror image of a tree
mirror :: Tree a -> Tree a
mirror Empty = Empty
mirror (Node left a right) = Node (mirror right) a (mirror left)