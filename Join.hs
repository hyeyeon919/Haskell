module Join (joinBy) where
-- Do NOT modify anything above this line.
-- Do NOT import anything.

{--
Given a list of strings and a length, return all strings that have the given 
length AND are made by joining three strings from the input list.

Note: The three strings are NOT required to be distinct and the same string 
can be used up to three times.

For example,

    ghci> joinBy 3 ["a", "b"]
    ["aaa", "aab", "aba", "abb", "baa",
    "bab", "bba", "bbb"]

    ghci> joinBy 4 ["a", "b", "bc", "cd", "d", "def"] 
    ["aabc", "aacd", "bbbc", "bbcd", "abbc", "abcd". "cddd", … ]
in the lasst example,
"aabc" is made of "a", "a" and bc;
"aacd" is made of "a", "a" and cd;
"bbbc" is made of "b", "b" and bc;
"abbc" is made of "a", "b" and bc;
"cddd" is made of "cd", "d" and d; and so forth

Note: The order of the resulting strings does not matter.
--}

--joinBy :: Int -> [String] -> [String]
--joinBy = undefined

--joinBy compareLength n $ joinStr input $ joinStr input input
--joinBy n
joinBy :: Integer -> [String] -> [String]
joinBy 0 _ = []
joinBy _ [] = []
--t = ["a", "b", "bc", "cd", "d", "def"]
joinBy n t  |n<3 = []
            |compareBool (joinStr t t) n == True = compareLength n $ joinStr t $ joinStr t t

joinStr :: [String] -> [String] -> [String]
joinStr xs ys = [(++) zs ws | ws <- xs, zs <- ys]

strLength :: [Char] -> Integer
strLength [] = 0
strLength (x:xs) = 1 + (strLength xs)

compareLength :: Integer -> [String] -> [String]
compareLength _ [] = []
compareLength n (x:xs)
        | n == (strLength $ x) = (compareLength n xs) ++ [x]
        | otherwise = (compareLength n xs)

compareBool :: [String] -> Integer -> Bool
compareBool [] _ = False
compareBool (x:xs) n
            |(strLength $ x) >= n = False
            |otherwise = True

