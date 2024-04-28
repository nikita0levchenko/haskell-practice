module ReaderMonad where

{-
newtype Reader r a = Reader{ getReader :: r -> a }

instance Monad (Reader r) where
  return vaule = Reader (\e -> value)
  m >>= k = Reader (\e -> let v = getReader m e in getReader (k v) e)
-}  