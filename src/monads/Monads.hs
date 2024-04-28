module Monads where

import Control.Monad (ap, liftM)


{-
 If we have type:

 f:: a -> m b
 where m - is some container with one parameter, then type above is Kleisly arrow
-}

data Log a = Log [String] a deriving Show

toLogger :: (a -> b) -> String -> (a -> Log b)
toLogger f str val = Log [str] (f val)

execLoggers :: a -> (a -> Log b) -> (b -> Log c) -> Log c
execLoggers val f g = Log allMsg gfval where
  Log fstMsg fval = f val
  Log sndMsg gfval = g fval
  allMsg = fstMsg ++ sndMsg

{-

class Monad m where
  return :: a -> m a
  (>>=) :: m a -> (a -> m b) -> m b

  infixl 1 >>=

-}

add1Log = toLogger (+1) "added one"

mult2Log = toLogger (* 2) "multiplied by 2"

bindLog :: Log a -> (a -> Log b) -> Log b
bindLog (Log msg val) f = Log allMsg res where
  Log resMsg res = f val
  allMsg = msg ++ resMsg

returnLog :: a -> Log a
returnLog = Log []

instance Monad Log where
  return = returnLog
  (>>=) = bindLog

instance Functor Log where
  fmap = liftM

instance Applicative Log where
  pure = return
  (<*>) = ap

execLoggersList :: a -> [a -> Log a] -> Log a
execLoggersList val = runFuncs (return val) where
  runFuncs :: Log a -> [a -> Log a] -> Log a
  runFuncs state [] = state
  runFuncs state (x : xs) = runFuncs ((>>=) state x) xs
  
execLoggersListFold :: a -> [a -> Log a] -> Log a
execLoggersListFold elem list = foldl (>>=) (return elem) list

{-

class Monad m where
  return :: a -> m a
  
  (>>=) :: m a -> (a -> m b) -> m b
  
  (>>) :: m a -> m b -> m b
  x >> y = x >>= \_ -> y
  
  fail :: String -> m a
  fail s = error s
  
  infixl 1 >>=
  
  MONAD LAWS
  
  1) (return value) >>= f == f value                - left identity
  
  2) Ma >>= return == Ma                            - right identity
  
  3) (Ma >>= k) >>= k' == Ma >>= (\x -> k x >>= k') - associative 
     Ma >>= k >>= k' == Ma >>= \x -> k x >>= k'
  
  

-}   