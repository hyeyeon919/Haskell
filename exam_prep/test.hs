collatz :: Int -> Int
collatz n
    | n == 1 = 1
    | mod n 2 == 0 = collatz y
    | mod n 2 == 1 = collatz x
    where
    x = ((div n 3) + 1)
    y = (div n 2)

check :: Int -> Bool
check n
    | n == 0 = True
    | otherwise = False

checkPattern :: Int -> Bool
checkPattern 0 = True
checkPattern _ = False

--리스트에서 가장 높은 빈도수를 가지는 요소를 출력한다.
mode :: (Eq a) => [a] -> a
mode input = fst (head $ getFreqEle $ freq input [(head input, 0)])

x = ["a","b","b"]

--입력 리스트를 tail recursion으로 빈도수 사전으로 만드는 과정
freq :: (Eq a) => [a] -> [(a, Int)] -> [(a, Int)]
freq (input : inputs) dictionary
    | inputs == [] = searchDictionary input dictionary
    | otherwise = freq inputs $ searchDictionary input dictionary

--입력 리스트의 한 원소를 기준으로 빈도수 사전을 tail recursion으로 순회해 빈도수를 계산하는 과정
searchDictionary :: (Eq a) => a -> [(a, Int)] -> [(a, Int)]
searchDictionary a (d : ds)
    | a == fst d = (a, 1 + snd d) : ds
    | length (ds) == 0 = d : [(a, 1)]
    | a /= fst d = d : searchDictionary a (ds)

--튜플의 빈도수의 최댓값을 찾는 과정
getFreqEle :: [(a, Int)] -> [(a, Int)]
getFreqEle (r : rs)
    | length rs == 0 = [r]
    | snd r > snd (head rs) = r : getFreqEle rs
    | otherwise = getFreqEle rs

split :: [Char] -> [[Char]]
split ("") = []
split (' ' : xs) = split $ dropWhile (==' ') xs
split xs = (takeWhile (/=' ') xs) : split (dropWhile(/=' ') xs)

--수소 출력
prime :: [Int] -> [Int]
prime (x:xs)
    | length (isPrime x) == 0 = x : prime xs
    | otherwise = prime xs

isPrime :: Int -> [Int]
isPrime x = [a | a <- [2..x-1], (==) 0 $ mod x a]

keys :: (Eq a) => Int -> [(a,Int)] -> [a]
keys value tab = [key | (key, val) <- tab, value == val]

evens :: Int -> Bool
evens 0 = True
evens x = odds $ x - 1

odds :: Int -> Bool
odds 0 = False
odds x = evens $ x - 1

evenList :: [a] -> [a]
evenList [] = undefined
evenList [x] = []
evenList (x : xs) = oddList xs

oddList :: [a] -> [a]
oddList [] = []
oddList [x] = [x]
oddList (x : xs) = x : evenList xs

drops :: Int -> [a] -> [a]
drops 0 [] = []
drops 0 xs = xs
drops n [] = []
drops n (x : xs) = drops (n-1) xs

takes :: Int -> [a] -> [a]
takes 0 [] = []
takes 0 xs = []
takes n [] = []
takes n (x : xs) = x : takes (n-1) xs

powset :: [a] -> [[a]]
powset [] = [[]]
powset (x : xs) = [x : y | y<-set] ++ powset xs
    where
    set = powset xs

quick :: Ord a => [a] -> [a]
quick [] = []
quick (x : xs) = quick left ++ [x] ++ quick right
    where
    right = [r | r <- xs, r > x]
    left = [l | l <- xs, l <= x]

listMin :: [Int] -> Int
listMin (x : xs)
    | (xs) == [] = maxBound :: Int
    | x < listMin xs = x
    | otherwise = listMin xs

subsum :: [Integer] -> Integer -> Bool
subsum [] 0 = True
subsum [] _ = False
subsum (x : xs) k = (subsum xs $ k-x) || (subsum xs k)


getLongestCommon :: Eq a => [a] -> [a] -> [a]
getLongestCommon x y = getMaxLength $ longestList x y

--겹치는 부분 리스트 출력
longestList :: Eq a => [a] -> [a]-> [[a]]
longestList [] _ = []
longestList (x:xs) y = comp (x:xs) a : longestList (xs) y
    where
    a = dropWhile (/=x) y

comp :: Eq a => [a] -> [a]-> [a]
comp [] _ = []
comp _ [] = []
comp (x:xs) (k:ks)
    | x == k = x : comp xs ks
    | otherwise = []

--부분 리스트중에서 길이가 가장 긴 리스트 출력
getMaxLength :: [[a]] -> [a]
getMaxLength (x:xs)
    | length (xs) == 0 = x
    | length x > length (getMaxLength xs) = x
    | otherwise = getMaxLength xs


alls :: (a->Bool) -> [a] -> Bool
alls p xs = foldr (&&) True $ map p xs

data Nat = Zero | Succ Nat deriving Show
add :: Nat -> Nat -> Nat
add m Zero = m
add m (Succ n) = add (Succ m) n

sub :: Nat -> Nat -> Nat
sub m Zero = m
sub (Succ m) (Succ n) = sub m n

validate :: [Char] -> Bool
validate x = all (\n->elem n $ ['a'..'z']) x

maybeFac :: Int -> Maybe Int
maybeFac x
    | x >= 0    = Just (helperFac x)
    | otherwise = Nothing
helperFac :: Int -> Int
helperFac x
    | x == 0 = 1
    | otherwise = x * helperFac (x-1)

isJusts :: Maybe a -> Bool
isJusts (Just a) = True
isJusts _ = False

data Tree a = Leaf a | Node (Tree a) a (Tree a) deriving Show
instance Functor Tree where
    fmap f (Leaf x)         = Leaf (f x)
    fmap f (Node lt x rt)   = Node (fmap f lt) (f x) (fmap f rt)

tree = Node ( Node( Leaf 1) 2 (Leaf 3)) 4 (Leaf 5)



