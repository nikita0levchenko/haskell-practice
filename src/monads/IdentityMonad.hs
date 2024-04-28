module IdentityMonad where
import Control.Monad (ap)  
  
{-

newtype Identity a = Identity {runIdentity :: a} deriving (Eq, Show)

instance Monad Identity where
  return x = Identity x
  (>>=) (Identity x) f = f x

return :: a -> m a
(>>=) :: m a -> (a -> m b) -> m b

fmap :: Functor f => (a -> b) -> f a -> f b

-}

data SomeType a = This a | That a
  deriving Show

instance Monad SomeType where
  return x = This x
  (>>=) (This x) f = f x

instance Applicative SomeType where
  pure  = return
  (<*>) = ap
  
instance Functor SomeType where
  fmap f x = x >>= (\y -> return (f y))
    
{-
x :: SomeType a
f :: a -> b
(>>=) :: SomeType a -> (a -> SomeType c) -> SomeType c
x >>= f :: ???

f _ :: a -> b
return (f _) :: a -> SomeType b  - this is trivial Kleisly arrow

-}    