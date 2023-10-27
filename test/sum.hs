joinBy :: [String] -> [String]
joinBy [] = []
joinBy (n : ns) = ([n] ++ (joinBy ns))::String
