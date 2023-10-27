f x [ys] z g = head $ g [x]

foo x h = h x
mystery = map (foo Nothing)

currytest = uncurry fst (((+1), [0,1,2]),3)
--fst ((+1), [0,1,2]) 3 = 4
idem :: Eq a => (a -> a -> a) -> (a -> Bool)
idem opFoo x = x `opFoo` x == x

opFoo :: Integer -> Integer -> Integer
opFoo x y = x

avg :: [Int] -> Int
avg x =
    let a = sum x
        b = length x
    in (div a b)

fooSing xs = foldr (++) [] (map sing xs) where sing x = [x]

fooSing2 xs = xs

pyth = [(a,b,c) | a <- [1..100], b <- [a..100], c <- [b..100], a^2 + b^2 == c^2]
pyth1 = [ (a,b,c) | c <- [1..100], b <- [1..c], a <- [1..b], a^2 + b^2 == c^2 ]

data List a = Val a | Set (List a) (List a) deriving Show
instance Functor List where
    fmap f (Val a) = Val (f a)
    fmap f (Set (lb) (lc)) = Set (fmap f lc) (fmap f lb)

--data Tree a = Leaf a | Node (Tree a) a (Tree a) deriving Show
--instance Functor Tree where
--    fmap f (Leaf x)         = Leaf (f x)
--    fmap f (Node lt x rt)   = Node (fmap f lt) (f x) (fmap f rt)
annoyUser :: IO()
annoyUser = do
    input <- getLine
    if input == "bingo" then
        return ()
    else
        do
        annoyUser

--sublist :: Eq a => [a] -> [a] -> Bool
sublist [] _ = True
sublist (x:xs) ys
    | elem x ys = sublist xs (dropWhile (==x) ys)
    | otherwise = False

least :: [Char] -> Char
least (x:xs) = h_least xs x

h_least:: [Char] -> Char -> Char
h_least [] ans = ans
h_least (x:xs) ans
    | x > ans = h_least xs ans
    | x <= ans = h_least xs x

data Expr = Value Int | Add Expr Expr deriving Show
folde :: (Int -> a) -> (a -> a -> a) -> Expr -> a
folde f _ (Value x) = f x
folde f g (Add e1 e2) = g (folde f g e1) (folde f g e2)

eval :: Expr -> Int
eval (Value x) = x
eval e = folde id (+) e

count ::Expr -> Int
count (Value x) = 1
count expr = folde (\x -> 1) (+) expr

makeUnique :: Eq a => [a] -> [a]
makeUnique [] = []
makeUnique [x] = [x]
makeUnique (x:xs)
    | elem x xs = makeUnique xs
    | otherwise = x : makeUnique xs
