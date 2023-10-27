powset :: [a] -> [[a]]

powset [] = [[]]
powset (x:xs) = yss ++ [x:ys | ys <- yss]
        where yss = powset xs