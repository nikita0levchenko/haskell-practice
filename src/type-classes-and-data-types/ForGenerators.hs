module ForGenerators where

import GHC.Float.RealFracMethods
import Data.Foldable
import Data.List

-- Define an infinitive list of 1
ones = 1 : ones

nuts :: Integer -> [Integer]
nuts n = n : nuts (n + 1)

{-
You can work with lazy inf functions like this:
take 5 $ nuts 5 -> [5, 6, 7, 8, 9]
-}

fibStream :: [Integer]
fibStream = 0 : 1 : zipWith (+) fibStream (tail fibStream)

-- some methods to create inf lists

take5z = take 5 $ repeat 'z'

replicateZ num = replicate num 'z'

cyclic5by5Z = take 25 $ cycle "zzzzz"

increment5 = take 5 $ iterate (\x -> x + 1) 0

-- create list from 0 to 10
tenList = [0..10]

tenListWithTwoStep = [0,3..10]

take5inf = take 5 $ [1,2..]

data Odd = Odd Integer
  deriving (Eq, Show)
   
instance Enum Odd where
  succ (Odd x) = Odd (x + 2)
  pred (Odd x) = Odd (x - 2)
  toEnum x = Odd (toInteger x * 2 + 1)
  fromEnum (Odd x) = quot (fromInteger x - 1) 2
  enumFrom = iterate succ
  enumFromThen (Odd x) (Odd y) = map Odd [x, y ..]
  enumFromTo (Odd x) (Odd y) = map Odd [x, x + 2 .. y]
  enumFromThenTo (Odd x) (Odd y) (Odd z) = map Odd [x , y .. z]
  
-- for comprehension
xs = [1 .. 20]
xsResult = [x ^ 2| x <- xs, x^2 < 200]
pairs = [(x, y) | x <- [1,2], y <- [1,2]]
pifagorTrio n = [(x, y, z) | x <- [1 .. n], y <- [1 .. n], z <- [1 .. n], x^2 + y^2 == z^2]
  

someFold :: (a -> b -> b) -> b -> [a] -> b
someFold f acc [] = acc
someFold f acc (x: xs)  = x `f` someFold f acc xs

meanList :: [Double] -> Double
meanList list = let (s, l) = foldr (\x (c, count) -> (c + x, count + 1)) (0, 0) list in s / l

evenOnly :: [a] -> [a]
evenOnly lst = foldr (\(x, y) acc -> if even y then x : acc else acc) [] (zippedList) where
  zippedList = zip lst [1 .. ]
  
lastElem :: [a] -> a
lastElem = foldl1 (\x y -> y)

revRange :: (Char,Char) -> [Char]
revRange (a, z) = unfoldr g z
  where g x = if x >= a then Just(x, pred x) else Nothing