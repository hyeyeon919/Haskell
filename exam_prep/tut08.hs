instance Functor [] where
    fmap _ [] = []
    fmap g (x:xs) = g : (fmap g xs)

instance Applicative [] where
-- pure :: a -> [a]
    pure f = repeat f
    [] <*> _ = []
    _ <*> [] = []
    (f:fs) <*> (x:xs) = (f x) : (fs <*> xs)