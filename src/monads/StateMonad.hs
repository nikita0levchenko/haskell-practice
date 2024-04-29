module StateMonad where

import Control.Monad (replicateM, ap, liftM)

newtype State s a = State {runState :: s -> (a, s)}

-- runState :: State s a -> s -> (s, a)

instance Monad (State s) where
  return a = State (\s -> (a, s))
  m >>= k = State $ \s ->
    let
    (a, s') = runState m s
    m'= k a
    in runState m' s'


execState :: State s a -> s -> s
execState m s = snd (runState m s)

evalState :: State s a -> s -> a
evalState m s = fst (runState m s)

get :: State s s
get = State (\s -> (s, s))

put :: s -> State s ()
put s = State (\_ -> ((), s))

tick :: State Int Int
tick = do
  n <- get
  put (n + 1)
  return n
  
modyfy :: (s -> s) -> State s ()
modyfy f = State (\s -> ((), f s))  

modyfyM :: (s -> s) -> State s ()
modyfyM f = do
  state <- get
  put (f state) 
  
succ' :: Int -> Int
succ' n = execState tick n  

instance Applicative (State s) where
  pure  = return
  (<*>) = ap

instance Functor (State s) where
  fmap = liftM
  
{-
replicateM :: (Monad m) => Int -> m a -> m [a]
replicateM n = sequence . replicate n
-}

fibStep :: State (Integer, Integer) ()
fibStep = do
  (a, b) <- get
  put (b, a + b)  
  
execStateN :: Int -> State s a -> s -> s
execStateN n m = execState (replicateM n m)
  
data Tree a = Leaf a | Fork (Tree a) a (Tree a) deriving Show  
  
numberTree :: Tree () -> Tree Integer
numberTree tree = evalState (loop tree) 1 where
  loop (Leaf _) = do
    n <- get
    modyfy n
    return (Leaf n)
  loop (Fork l _ r) = do
    l' <- loop l
    n <- get
    modyfy n
    r' <- loop r
    return (Fork l' n r')
      