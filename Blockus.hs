module Blockus (tile) where

-- Do not modify anything above this line.
--
-- This question is worth 10 POINTS

{--
Implement the tiling from Q10 in the Written Assignment.

Implement function 
	tile :: Int -> [[Int]]

which will construct the tiling of a board 2^k x 2^k for the given k.
If k is less than zero, return an empty list.
In the output, each V3 piece should be assigned a unique positive non-zero number. 
The upper left corner should be assigned a 0. 
For example, the tiling for a 4x4 Board might look like this:

0 1 4 4 
1 1 2 4
5 2 2 3
5 5 3 3

So, 
    tile 2 == [[0, 1, 4, 4]
              ,[1, 1, 2, 4] 
              ,[5, 2, 2, 3] 
              ,[5, 5, 3, 3]]

Note that this solution is not unique. Any valid solution will be accepted.

--}

tile :: Int -> [[Int]]
tile a
    | a < 0 = []
    | a == 0 = [[0]]
    | a == 1 = [[0,1], [1,1]]
    | otherwise = tilemake (tile $ a-1)

maketile :: [[Int]] -> [[Int]] -> [[Int]]
maketile [] [] = []
maketile (x:xs) (y:ys) = [x ++ y] ++ maketile xs ys

addOne :: [[Int]] -> Int -> [[Int]]
addOne arr ele = map (\row -> map (+ ele) row) arr

tilemake :: [[Int]] -> [[Int]]
tilemake a = maketile a b ++ maketile d c
    where
    ele = head (last  a)
    c = addOne a (ele + 1) -- C
    b = rotate (addOne c (ele)) -- B
    d = rotate (rotate (rotate (addOne c (2 * ele))))

rotate :: [[Int]] -> [[Int]]
rotate = reverse . transpose

transpose :: [[Int]] -> [[Int]]
transpose ([]:_) = []
transpose x = (map head x) : transpose (map tail x)
