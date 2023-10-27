module FunWithMonads 
  ( Env (..)
  , proj1
  , proj2
  , runEnv
  , correlation
  ) where


-- This task is worth 20 POINTS
-- Do NOT modify anything above this line.
-- Do NOT use any imports.


-- You are given the type:

data Env r1 r2 a = Env (r1 -> r2 -> a)

{--

Part 1.

Implement the following instances:

--}

instance Functor (Env r1 r2) where
    fmap f (Env g) = Env (\r1 r2 -> f (g r1 r2))

instance Applicative (Env r1 r2) where
    pure x = Env (\_ _ -> x)
    (Env f) <*> (Env g) = Env (\r1 r2 -> (f r1 r2) (g r1 r2))

instance Monad (Env r1 r2) where
    return = pure
    (Env g) >>= f = Env (\r1 r2 -> let Env h = f (g r1 r2) in h r1 r2)

{--
Part 2.

Env emulates two global environments that can not be modified but can be accessed at any time. 

Implement proj1 :: (r1 -> a) -> Env r1 r2 a which will return a projection of its first environment.
Implement proj2 :: (r2 -> a) -> Env r1 r2 a which will return a projection of its second environment.
Implement runEnv :: Env r1 r2 a -> (r1 -> r2 -> a) which will transform the computation inside Env into a Haskell function.

Using do-notation and, possibly, the functions defined above, implement
correlation :: Env [Float] [Float] Float

which will calculate Pearson correlation between two lists.
The lists are guaranteed to be of the same length greater than one with distinct elements, i.e. the correlation will never be undefined. 
Due to floating point imprecision, your answer will be accepted if it is within 0.001 of the correct answer.

                   Σ(x_i - x̄) * (y_i - ȳ)
   pearson =  -------------------------------
              √ (Σ(x_i - x̄)^2 * Σ(y_i - ȳ)^2)

For example, 
    
    runEnv correlation [1, 11, 23, 42, 1, 2, 5, 90, 22, 65] [228, 14, 98, 33, 17, 0, -1, 33.5, 10, 6] == -0.1967 (± 0.001)

--}

proj1 :: (r1 -> a) -> Env r1 r2 a
proj1 f = Env (\r1 _ -> f r1)

proj2 :: (r2 -> a) -> Env r1 r2 a
proj2 g = Env (\_ r2 -> g r2)

runEnv :: Env r1 r2 a -> (r1 -> r2 -> a)
runEnv (Env f) = f

correlation :: Env [Float] [Float] Float
correlation = do
  xs <- proj1 (\r1 -> r1)
  ys <- proj2 (\r2 -> r2)
  calculateCorrelation xs ys

calculateCorrelation :: [Float] -> [Float] -> Env [Float] [Float] Float
calculateCorrelation xs ys = do
      let meanX = sum xs / fromIntegral (length xs)
          meanY = sum ys / fromIntegral (length xs)
          a = sum [(x - meanX) * (y - meanY) | (x, y) <- zip xs ys]
          b = sqrt (sum [(x - meanX) ^ 2 | x <- xs]) * sqrt (sum [(y - meanY) ^ 2 | y <- ys])
      return (a / b)
