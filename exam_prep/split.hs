split [] = []
split (' ':xs) = split xs
split xs = takeWhile (/=' ') xs : (split $ dropWhile (/=' ') xs)