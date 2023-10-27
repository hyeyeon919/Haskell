--map :: (a -> b) -> [a] -> [b]
--입력 배열의 원소마다 함수를 적용시켜 배열로 출력
--map함수를 list comprehension으로 구현
maplist :: (a -> b) -> [a] -> [b]
maplist f xs = [f x | x <- xs]
--함수와 함수를 적용시킬 변수를 입력으로 받는다.
--입력 배열의 각 원소에 함수를 적용시키기 위해서 list comprehension을 이용한다.
--recursion을 이용한 map함수 구현
mapre :: (a -> b) -> [a] -> [b]
mapre _ []    = []
mapre f (x : xs) = (f x : mapre f xs)

--zip :: [a] -> [b] -> [(a,b)]
--두개의 리스트를 한개의 튜플 리스트로 만드는 함수
ziplist :: [a] -> [b] -> [(a,b)]
ziplist [] _ = []
ziplist _ [] = []
ziplist (x:xs) (y:ys) = [(x,y)] ++ ziplist xs ys
--ziplist (x:xs) (y:ys) = (x,y) : ziplist xs ys

--zipWith :: (a -> b ->c) -> [a] -> [b] -> [c]
--첫번째 리스트와 두번째 리스트를 입력 함수연산을 적용해 새로운 배열을 출력한다.
zipWithlist :: (a -> b ->c) -> [a] -> [b] -> [c]
zipWithlist _ [] _ = []
zipWithlist _ _ [] = []
zipWithlist f (x:xs) (y:ys) = (f x y) : (zipWithlist f xs ys)

--filter :: (a -> Bool) -> [a] -> [a]
--리스트의 원소중에서 조건에 만족하는 원소들만 리스트에 담아 출력하는 함수
filterlist :: (a -> Bool) -> [a] -> [a]
filterlist _ [] = []
filterlist f (x:xs)
    | f x == True = (x : filterlist f xs)
    | otherwise = filterlist f xs

--all :: Foldable t => (a -> Bool) -> t a -> Bool
--리스트의 모든 원소가 해당 조건을 만족하는지 확인한다.
alllist :: (a -> Bool) -> [a] -> Bool
--list are foldable
alllist _ [] = True
alllist f (x:xs) = f x && alllist f xs
--tail recursion으로 head의 원소가 참인지 확인한다.
--나머지 모든 원소들의 boolean값을 && 로 연결해 모두 참일때 참을 출력하도록한다.

--any :: Foldable t => (a->Bool) -> t a -> Bool
--리스트의 원소중에서 조건에 만족하는 원소가 하나라도 존재할때 참을 반환한다.
anylist :: (a -> Bool) -> [a] -> Bool
anylist _ [] = False
anylist f (x:xs) = f x || anylist f xs

--all과 any함수를 filter함수를 이용해서 구현한다.
--filter함수 해당 조건에 만족하는 원소들만 출력한다.
--filter :: (a -> Bool) -> [a] -> [a]
--all :: Foldable t => (a -> Bool) -> t a -> Bool
allfilter f x = filter f x == x
--any :: Foldable t => (a->Bool) -> t a -> Bool
anyfilter f x = filter f x /= []

--folding은 이항 함수를 반복적으로 적용하는 것이다.
--fold는 spine을 대체하기 위한 high order function이다.
--fold (^) [1,2,3] = 1^2^3이지만 (1^2)^3연산을 수행하고 싶을떄 foldr과 foldl을 이용해 우선순위를 부여할 수 있다.
-- foldr (^) 2 [1,2] = 1^2^2 = 1
-- foldl (^) 2 [1,2] = 2^2^1 = 4
--리스트의 원소의 총 합
--sum = foldr (+) 0
--prod = foldr (*) 1
--cat foldr (++) []
--초기값을 주고 리스트를 연산자와 함께 append를 하면서 식을 만들어 계산한다.

--all :: Foldable t => (a -> Bool) -> t a -> Bool
--map :: (a -> b) -> [a] -> [b]
--foldl :: (b -> a -> b) -> b -> [a] -> b
--foldr :: (a -> b -> b) -> b -> [a] -> b
--fold와 map을 이용해 all을 구현
allfoldmap :: (a -> Bool) -> [a] -> Bool
allfoldmap f x = foldr (&&) True $ map f x

--fold를 이용해 length를 구현
lengthfold :: [Int] -> Int
lengthfold [] = 0
lengthfold (x:xs) = foldr (+) 0 $ [lengthfold xs]++[1]