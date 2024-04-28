module MyList where
  
{-
class Monad m where
  return :: a -> m a
  (>>=) :: m a -> (a -> m b) -> m b
  (>>) :: m a -> m b -> m b
  fail :: String -> m a
  
instance Monad List where
  return x = [x]
  (>>+) [] _ = []
  (>>=) (x : xs) f = (f x) ++ (>>= xs f)
  fail _ = []  
 
-}

checkListBind :: [a] -> (a -> [b]) -> [b]
checkListBind [] _ = []
checkListBind (x : xs) f = f x ++ checkListBind xs f


pythagoreanTriple :: Int -> [(Int, Int, Int)]
pythagoreanTriple x 
  | x <= 0 = []                    
  | otherwise = do
    b <- [1 .. x]
    a <- [1 .. b]
    c <- [1 .. x]
    True <- return (a^2 + b^2 == c^2 && a > 0 && b > 0 && c > 0 && c <= x && a < b)
    return (a, b, c)
  
