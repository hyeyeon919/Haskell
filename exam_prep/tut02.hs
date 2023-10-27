q0 f [a] = f a

q2 (a,b) (c,d) = ((b,d),(a,c))

q3 f1 f2 f3 = f2 $ (\x -> f2 (f3.f1))

q4 f g a = f a (g a)

q5 a1 [b] a2 f = head (f [a1])

q6 [a] t f = f (fst t) [a]

q7 f1 f2 = f2 $ f1 (\x y -> f2 x)