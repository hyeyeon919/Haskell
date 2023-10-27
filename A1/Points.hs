module Points (closest, npoints, manhattan) where

import           Prelude hiding (liftA2, liftM2, pure, return, sequence,
                          sequenceA, sequenceM, (*>), (<*>), (>>), (>>=))
-- Do NOT modify anything above this line.
-- Do NOT import anything.

{--
You are given N lists of integer coordinates. Without using list comprehensions,
do-notation or any functions from Control.Applicative and Control.Monad, 
implement an algorithm that constructs all possible N-dimensional points from 
the given coordinates and finds Manhattan distance between two closest distinct 
points.

The Manhattan distance of two N-dimensional points is calculated as follows:
    manhattan [x1, x2, x3, ..., xn] [y1, y2, y3, ..., yn] 
    = abs (x1 - y1) + abs (x2 - y2) + ... + abs (xn - yn)

TASK 1: Write a function
    manhattan :: [Int] -> [Int] -> Int
that returns the Manhattan distance of two coordinates represented by integer
lists.
    ghci> manhattan [0, 3, 1] [0, 2, 1] 
    1

TASK 2: Write a function
    npoints :: [[Int]] -> [[Int]]
that generates all possible points constuctable from its first input. That is,
supposing
    npoints [x1s, x2s, ..., xks] = ans
then [x1, x2, ..., xk] in ans iff x1 in x1s, x2 in x2s, ..., and xk in xks.

For example, 
    ghci> npoints [[0, 1], [2, 3], [1, 3]]
    [[0,2,1],[0,2,3],[0,3,1],[0,3,3],[1,2,1],[1,2,3],[1,3,1],[1,3,3]]

TASK 3: Write a function
    closest :: [[Int]] -> Int
that returns the least Manhattan distance among all the input points.

For example:
    ghci> closest $ npoints [[0, 1], [2, 3], [1, 3]]
    1

Because [0,3,1] and [0,2,1] have Manhattan distance 1.

In order to make this function total, we let
    closest xs = -1
when len xs < 2.

Note: the order of points does not matter, but the order of coordinates in each 
point does. The i-th coordinate of a point should always come from the i-th list
 in the input.
--}

manhattan :: [Int] -> [Int] -> Int
manhattan [] [] = 0
manhattan (x:xs) (y:ys) = abs(x-y) + manhattan xs ys



npoints :: [[Int]] -> [[Int]]
npoints [[]] = []
npoints [] = []
npoints (x:xs)      |   xs == [] = singleEle x
                    |   getTail xs == [] = first (getHead (x:xs)) (getTail (x:xs))
                    |   otherwise = afterfirst (first (getHead (x:xs)) (getTail (x:xs))) (getTail $ getTail (x:xs))

--function to cover npoints with a single point.
singleEle :: [Int] -> [[Int]]
singleEle [] = []
singleEle (x:xs) = [[x]] ++ singleEle xs

--adding first, second element together in to a list.
first :: [Int] -> [[Int]] -> [[Int]]
first [] _ = []
first _ [] = []
first (x:xs) (y:ys) = appendy x y ++ first xs ([y]++ys)

--helper used to implement first
appendy :: Int -> [Int] -> [[Int]]
appendy _ [] = []
appendy x (y:ys) = [[x] ++ [y]] ++ appendy x ys

--adding elements from third elements into lists
afterfirst :: [[Int]] -> [[Int]] -> [[Int]]
afterfirst _ [] = []
afterfirst x (y:ys) = appending x y ++ afterfirst x ys

--helper used to implement afterfirst
appending :: [[Int]] -> [Int] -> [[Int]]
appending _ [] = []
appending x (y:ys) = appending2 x y ++ appending x ys

appending2 :: [[Int]] -> Int -> [[Int]]
appending2 [] _ = []
appending2 (x:xs) y = [x ++ [y]] ++ appending2 xs y



closest :: [[Int]] -> Int
closest [[]] = -1
closest [] = -1
closest (input:inputs)      | (inputs) == [] = -1
                            | otherwise = min_f (comp (input:inputs))

--comp function to make the distance into a list
comp :: [[Int]] -> [Int]
comp [] = []
comp (x:xs) = mid x xs ++ comp xs

--helper used to implement comp
mid :: [Int] -> [[Int]] -> [Int]
mid _ [] = []
mid x (y:ys) = [manhattan x y] ++ mid x ys

--custom tail, head to avoid errors
getTail :: [[Int]] -> [[Int]]
getTail [[]] = []
getTail [] = []
getTail (x:xs) = xs

getHead :: [[Int]] -> [Int]
getHead [[]] = []
getHead [] = []
getHead (x:xs) = x

--custom getMin to avoid error
min_f :: [Int] -> Int
min_f []       = 0
min_f [x]      = x
min_f (x:xs)   = getMin x (min_f xs)

getMin :: Int -> Int -> Int
getMin a b
    | a > b  = b
    | a <= b  = a

