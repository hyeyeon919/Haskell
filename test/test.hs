q5 a1 (b:bs) a2 f = head (f [a1, a2])

q6 (a:as) (b,c) f = f b as

--assignment

f1 (f,a) = f a

f2 a (b,c) = b

f3 f a = [f a]

f4 f1 f2 a = f1 (f2 a)

f5 f a b c = f (a,b,c)

f6 f (a,b,c) = f a b c