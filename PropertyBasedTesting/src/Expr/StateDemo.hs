{-# LANGUAGE InstanceSigs #-}
module Expr.StateDemo where

import Control.Monad ( replicateM )

newtype State s a = State { runState :: s -> (s, a) }

instance Functor (State s) where
    fmap :: (a -> b) -> State s a -> State s b
    fmap f x = State $ \s ->
      let (s', y) = runState x s in
      (s', f y)

instance Monad (State s) where
    (>>=) :: State s a -> (a -> State s b) -> State s b
    m >>= k = State $ \s ->
      let (s', x) = runState m s in
      runState (k x) s'

instance Applicative (State s) where
    pure :: a -> State s a
    pure x = State $ \s -> (s, x)

    (<*>) :: State s (a -> b) -> State s a -> State s b
    f <*> x = State $ \s ->
      let (s', f') = runState f s in
      let (s'', x') = runState x s' in
      (s'', f' x')

execState :: State s a -> s -> a
execState m x = snd (runState m x)

get :: State s s
get = State $ \s -> (s, s)

put :: s -> State s ()
put s = State $ \_ -> (s, ())

increment :: State Int Int
increment = do
  n <- get 
  put (n+1)  
  return n  

modify :: (s -> s) -> State s ()
modify f = do
  s <- get
  put (f s)

-- replicateM :: Applicative m => Int -> m a -> m [a]
-- replicateM n act performs the action act n times, and then returns the list of results:
efficientFib :: Int -> Int
efficientFib n =
    last $ execState (replicateM n step) (0,1)
  where
    step = do
      (a, b) <- get
      put (b, a+b)
      return b

-- type -- type synonym 
type Random a = State Integer a

nextNumber :: Integral a => a -> a
nextNumber n =  (6364136223846793005 * n + 1442695040888963407) `mod` (2^64) 

-- -- This function returns the new random number, whereas the next function returns the current one
-- fresh :: Random Integer 
-- fresh = do 
--   n <- get 
--   let next = (nextNumber n :: Integer)
--   put next 
--   return next 

fresh :: Random Integer
fresh = do 
  n <- get 
  modify nextNumber 
  return n  

runPRNG :: Random a -> Integer -> a
runPRNG = execState 