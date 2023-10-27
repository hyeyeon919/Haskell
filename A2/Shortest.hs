module Shortest (shortest) where

-- Do not modify anything above this line.
--
-- This question is worth 10 POINTS

{--
shortest is a function that receives a list of lists and returns the shortest 
list if there is one, and Nothing otherwise.

	shortest :: [[a]] -> Maybe [a]

Your implementation must satisfy the following requirements:
1. It must work even if there are infinite lists in the input (see *).
2. It must work even if the input list itself is infinite (see *).
3. If there are several lists with the minimal length, return the first of them.

	* We guarantee that for all test inputs (visible and hidden), it will be 
possible to find a solution within a reasonable amount of time using an 
appropriate algorithm. For example, there won’t be any infinite lists of 
infinite lists in the tests.

For example,
        shortest  [[1, 3..], [10..], repeat 5, [2, 4], [2, 4..], [42, 228]]
            == Just [2, 4]
--}


shortest :: [[a]] -> Maybe [a]
shortest [] = Nothing
shortest xs = Just (mapping [1..] xs)

(n:ns) = [1..]
mapping :: [Int] -> [[a]] -> [a]
mapping (n:ns) input
    | length (mincheckoutput) == 1 = head mincheckoutput
    | otherwise = remapping
    where
    mapinput = map (take (n * 10)) input
    mincheckoutput = mincheck (mapinput) [head mapinput]
    remapping = mapping ns input

mincheck :: [[a]] -> [[a]] -> [[a]]
mincheck (x:xs) minx
    | length xs == 1 = minx
    | length x > length (head xs) = mincheck (tail xs) [head xs]
    | length x == length (head xs) = mincheck (tail xs) ([x]++[head xs])
    | length x < length (head xs) = mincheck (tail xs) [x]


