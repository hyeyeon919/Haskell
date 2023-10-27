data Shapes = Rectangle Float Float | Circle Float | Triangle Float Float deriving (Eq)

instance Ord Shapes where
    compare x y = compare (area x) (area y)
        where
            area (Circle r)         = 3.14 * r ** 2
            area (Rectangle l w)    = l * w
            area (Triangle b h)     = 1/2 * b * h


data List a = Empty | Connect a (List a)
instance Show a => Show (List a) where
    show Empty = "><"
    show (Connect x xs) = unwords [show x, "->", show xs]

len :: List a -> Int
len Empty = 0
len (Connect x xs) = len xs + 1

data Nat = Zero | Succ Nat deriving Show
zero = Zero
one = Succ zero
two = Succ one
three = Succ two

nat2int :: Nat -> Int
nat2int Zero = 0
nat2int (Succ n) = 1 + nat2int n

int2nat :: Int -> Nat
int2nat 0 = Zero
int2nat n = Succ $ int2nat $ n-1

add :: Nat -> Nat -> Nat
add m Zero = m
add m (Succ n) = add (Succ m) n

sub :: Nat -> Nat -> Nat
sub m Zero = m
sub (Succ x) (Succ y) = sub x y

data BinTree a = Leaf a | Node (BinTree a) a (BinTree a) deriving Show

preorder :: BinTree a -> [a]
preorder (Leaf a) = [a]
preorder (Node lTree x rTree) = [x] ++ preorder lTree ++ preorder rTree

inorder :: BinTree a -> [a]
inorder (Leaf a) = [a]
inorder (Node lTree x rTree) = inorder lTree++ [x] ++ inorder rTree




