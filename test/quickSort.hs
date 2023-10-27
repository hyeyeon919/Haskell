quicksort (x:xs) = ys ++ [x] ++ zs
    where
    ys = quicksort [y | y <- xs, y <= x]
    zs = quicksort [z | z <- xs, z > x]

