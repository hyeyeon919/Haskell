--#1
--1) show how the list comprehension [f x x <- xs, p x]| can be re-expressed using the higher-order functions map and filter

--2) Define takeWhile and dropWhile using higher order functions

--#2
--Consider the higher order function foo
--foo :: (t-> Bool) -> (t->a) -> (t->t) -> t -> [a]
--foo p h t x
--    | p x   = []
--    | otherwise = h x : foo p h t (t x)
--1) define takeWhile by equating it to a single invocation of foo

--2) define map by equating it to a single invocation of foo

--3) define iterate by equating it to a single invocation of foo
