module IOMonad where
import Data.List (isInfixOf, filter)
import Control.Monad(liftM)
import System.Directory (getDirectoryContents, removeFile)

main' :: IO ()
main' = do
  putStrLn "What is your name?"
  putStr "Name: "
  name <- getLine
  if null name then main' else putStrLn ("Hi, " ++ name ++ "!")

{-
class Monad m where
  return :: a -> m a
  (>>=) :: m a -> (a -> m b) -> m b
  (>>) :: m a -> m b -> m b
  fail :: String -> m a

newtype IO a = IO (RealWorld -> (RealWorld, a))
return :: a -> IO a | a -> RealWorld -> (RealWorld, a)
(>>=) IO a -> (a -> IO b) -> IO b
| (>>=) (RealWorld -> (RealWorld, a)) -> (a -> RealWorld -> (RealWorld, b)) -> RealWorld -> (RealWorld, b)


instance Monad IO where
  return a w = (w, a) | return a = \w -> (w, a)
  (>>=) m k = \w -> case m w of
    (w' a) -> k a w'
-}

getLine' :: IO String
getLine' = do
  c <- getChar
  if c == '\n' then return [] else do
    cs <- getLine'
    return (c : cs)

putStr' :: String -> IO ()
putStr' [] = return ()
putStr' (x : xs) = putChar x >> putStr' xs

{-
sequence_ :: Monad m => [m a] -> m ()
sequence_ = foldr (>>) (return ())

mapM_ :: Monad m => (a -> m b) -> [a] -> m ()
mapM_ = sequence_ . map f
-}

