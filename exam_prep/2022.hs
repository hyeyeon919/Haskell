--1
q1 = take 1000 [x | x <- [1..], (mod x 3 == 0 || mod x 5 == 0)]

--1-(c)
--foo_q3 = foldl (curry fst)
--foo_ans x y = x

--1-(d)
f [x] y g = g (fst y) [x]

--1-(e)
--q1_e xs ys= (\x -> (x, (\y -> y) <*> ys) <*> xs)

--1-(i)
--reverse_q1 x = foldl (++) [] x

--1-(j)
fooj::IO()
fooj = putStr ("Hello World")

--3
foo :: Int -> IO()
foo x = do
    putStr "prompt : "
    input <-readLn :: IO Int
    if (x - input) > 0 then
        foo $ (x - input)
    else
        putStrLn "Done"

addio ::IO()
addio = do
    ws <- fmap (words) getLine
    xs <- return [read w ::Int | w <- ws]
    print (sum xs)

--4
iter 0 f = id
iter n f = f . iter (n-1) f

--5
longest input = foldr (\x y -> lenthy x y) [] input

lenthy :: [Int] -> [Int] -> [Int]
lenthy x y
    | length x > length y = x
    | otherwise = y

powset [] = [[]]
powset (x:xs) = powset xs ++ [x:y | y <- powset xs]

lcs x y = longest [xl | xl <- xp, yl <- yp, xl==yl]
    where
        xp = powset x
        yp = powset y

--7
makeUnique xs = h_uni xs []

h_uni [] ans = ans
h_uni (x:xs) ans
    | elem x xs = h_uni xs ans
    | otherwise = h_uni xs (x:ans)

--8
--msums :: [Maybe Integer] -> Maybe Integer
msums ms = foldl (\x y -> (+) <$> x <*> y) (Just 0) ms

--9
data Expr = X | Num Int | BinOp Op Expr Expr deriving (Eq, Show)
data Op = Add | Mul | Subtract deriving (Eq, Show)

removeSub :: Expr -> Expr
removeSub (BinOp Subtract a b) = (BinOp Add (removeSub a )(BinOp Mul (Num (-1)) (removeSub b)))
removeSub (BinOp op e1 e2) = (BinOp op (removeSub e1) (removeSub e2))
removeSub e = e
