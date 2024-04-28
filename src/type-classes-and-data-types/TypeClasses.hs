module TypeClasses where

import Data.Typeable
import Prelude hiding (Eq, (/=), (==))

-- Create your own class types

class Eq a where
  (==) :: a -> a -> Bool
  (/=) :: a -> a -> Bool

instance Eq Bool where
  True == True = True
  False == False = True
  _ == _ = False
  x /= y = not (x == y)

class Printable a where
  toString :: a -> String

instance Printable Bool where
  toString True = "true"
  toString False = "false"

instance Printable () where
  toString () = "unit type"

instance (Printable a, Printable b) => Printable (a, b) where
  toString (a, b) = "(" ++ (toString a) ++ "," ++ (toString b) ++ ")"

-- Type class for polimorph types

instance (Eq a, Eq b) => Eq (a, b) where
  a == b = fst a == fst b && snd a == snd b

class KnownToGork a where
  stomp :: a -> a
  doesEnrageGork :: a -> Bool

class KnownToMork a where
  stab :: a -> a
  doesEnrageMork :: a -> Bool

class (KnownToGork a, KnownToMork a) => KnownToGorkAndMork a where
  stompOrStab :: a -> a
  stompOrStab a
    | doesEnrageGork a && doesEnrageMork a = (stomp . stab) a
    | doesEnrageMork a && not (doesEnrageGork a) = stomp a
    | doesEnrageGork a && not (doesEnrageMork a) = stab a
    | otherwise = a

{- You can specify particular type when you use polymorph functions

 >read "5" :: Int
 >5

 >read "[1,2,3]" :: [Double]
 >[1,2,3]

 >reads "5 rings" :: [(Int, String)]
 [(5,"rings")]
 -}

class (Enum a, Eq a, Bounded a) => SafeEnum a where
  ssucc :: a -> a
  ssucc a
    | a == maxBound = minBound
    | otherwise = succ a
  spred :: a -> a
  spred a
    | a == minBound = maxBound
    | otherwise = pred a

instance SafeEnum Bool

baz x = const True

foo a = a

bar = const foo

grault x 0 = x
grault x y = x

garply = grault 'q'

{-
nTimes:: a -> Int -> [a]
nTimes a n = loop a n []     
loop :: a -> Int -> [a] -> [a]
loop elem n acc | n == 0 = acc
                | otherwise = loop elem (n - 1) (elem : acc)
-}

