module Test where

import Data.Char

sayHello = putStrLn "Hello world from module Test!"

sumSquares x y = x ^ 2 + y ^ 2

lenVec3 x y z = sqrt (x ^ 2 + y ^ 2 + z ^ 2)

sign x = if x == 0 then 0 else if x > 0 then 1 else -1

max5 = max 5

maxOperatorStyle x y = x `max` y

sumInFunctionalStyle x y = (+) x y

--задание ассоциативности для операторов
-- infixl - лево-ассоциативный оператор
-- infixr - право-ассоциативный оператор
-- infix - оператор без ассоциативности

-- символы для операторов
{-
! # $ % & * + . / < = > ? @ \ ^ | - ~ :
-}

infixl 6 ^+^

a ^+^ b = a ^ 2 + b ^ 2

infix 6 |-|

a |-| b = if (a - b) >= 0 then a - b else (-1) * (a - b)

{-
Трюк как избавится от многих скобок

sin (pi / 2) => sin $ pi / 2
-}

discount :: Double -> Double -> Double -> Double
discount limit proc sum = if sum >= limit then sum * (100 - proc) / 100 else sum

standardDiscount :: Double -> Double
standardDiscount = discount 1000 5

checkChar :: Char -> Bool
checkChar char = isDigit char

twoDigits2Int :: Char -> Char -> Int
twoDigits2Int x y = if isDigit x && isDigit y then (digitToInt x) * 10 + digitToInt y else 100

dist :: (Double, Double) -> (Double, Double) -> Double
dist p1 p2 = sqrt ((fst p1 - fst p2) ^ 2 + (snd p1 - snd p2) ^ 2)

doubleFact :: Integer -> Integer
doubleFact 1 = 1
doubleFact 2 = 2
doubleFact n = if n < 0 then error "Can't count factorial of negative number" else n * doubleFact (n - 2)

modifiedDoubleFact :: Integer -> Integer
modifiedDoubleFact 0 = 1
modifiedDoubleFact 1 = 1
modifiedDoubleFact n
  | n < 0 = error "Can't count factorial of negative number"
  | n > 0 = n * modifiedDoubleFact (n - 2)

fibonacci :: Integer -> Integer
fibonacci 0 = 0
fibonacci 1 = 1
fibonacci (-1) = 1
fibonacci n
  | n > 0 = fibonacci (n - 1) + fibonacci (n - 2)
  | n < 0 = fibonacci (n + 2) - fibonacci (n + 1)
  | otherwise = undefined

newFactorial :: Integer -> Integer
newFactorial n
  | n > 0 = loop 1 n
  | otherwise = error "n must be >= 0"

loop :: Integer -> Integer -> Integer
loop acc 0 = acc
loop acc n = loop (acc * n) (n - 1)

newFibonacci :: Integer -> Integer
newFibonacci n
  | n == 0 = 0
  | otherwise = newLoop 0 1 n

newLoop :: Integer -> Integer -> Integer -> Integer
newLoop prev curr n
  | n == 0 = prev
  | n > 0 = newLoop curr (prev + curr) (n - 1)
  | n < 0 = newLoop curr (prev - curr) (n + 1)

roots :: Double -> Double -> Double -> (Double, Double)
roots a b c =
  let d = sqrt (b ^ 2 -4 * a * c)
   in ((- b - d) / (2 * a), (- b + d) / (2 * a))

roots' :: Double -> Double -> Double -> (Double, Double)
roots' a b c =
  let d = sqrt (b ^ 2 -4 * a * c)
      x1 = (- b - d) / (2 * a)
      x2 = (- b + d) / (2 * a)
   in (x1, x2)

seqA :: Integer -> Integer
seqA n =
  let loop a b c 0 = a
      loop a b c n = loop b c (c + b - (2 * a)) (n - 1)
   in loop 1 2 3 n

sum'n'count :: Integer -> (Integer, Integer)
sum'n'count n
  | n == 0 = (0, 1)
  | n < 0 = sum'n'count (- n)
  | otherwise = loop 0 0 n
  where
    loop x y z
      | z == 0 = (x, y)
      | otherwise = loop (x + (mod z 10)) (y + 1) (div z 10)

integration :: (Double -> Double) -> Double -> Double -> Double
integration f a b = loop a 1000 0
  where
    loop a n acc
      | n == 0 = acc
      | otherwise = loop (a + h) (n - 1) (acc + res)
      where
        h = (b - a) / n
        res = h * (f (a) + f (a + h)) / 2


f1 :: Int -> Char
f1 x = intToDigit x

f2 :: Char -> String
f2 x = x : " is a char"

swapCheck :: (a,b) -> (b,a)
swapCheck = uncurry (flip (,))
  
