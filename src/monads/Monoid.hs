module Monoid where

{-
 Monoid in math is set with defined associative binary operation on this set, 
 with neutral element related to this operation
-}
  
class MonoMonoid a where
  monempty :: a                        -- neutral element
  monoperation :: a -> a -> a          -- binary operation
  
  monoconcat :: [a] -> a
  monoconcat = foldr monoperation monempty
  
{-
 The monoid laws:
 1) monooperation monempty x == x
 2) monooperation x monempty == x
 3) (x `monooperation` y ) `monooperation` z == x `monooperation` (y `monooperation` z)
-}

instance MonoMonoid [a] where
  monempty = []
  monoperation = (++)
  
  
{- 
Endomorphism is:  a -> a

Also we have special type Endo, which is also a Monoid
-}

newtype Endo a = Endo {appEndo :: a -> a}
  