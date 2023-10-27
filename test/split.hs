split :: String -> [String]
split ("") = []
split (' ':xs) = split $ dropWhile (==' ') xs
split xs = (takeWhile (/=' ') xs):(split $ dropWhile (/=' ') xs)