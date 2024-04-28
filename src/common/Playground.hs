module Playground where

import Data.Char

nTimes :: a -> Int -> [a]
nTimes a n = loop n []
  where
    loop n acc
      | n == 0 = acc
      | otherwise = loop (n - 1) (a : acc)

oddsOnly :: Integral a => [a] -> [a]
oddsOnly [] = []
oddsOnly (x : xs)
  | odd x = x : oddsOnly xs
  | otherwise = oddsOnly xs

isPalindrome :: Eq a => [a] -> Bool
isPalindrome list = list == reverse list

sum3 :: Num a => [a] -> [a] -> [a] -> [a]
sum3 l1 l2 l3 = l1 `sum2` l2 `sum2` l3
  where
    sum2 [] ys = ys
    sum2 xs [] = xs
    sum2 (x : xs) (y : ys) = (x + y) : sum2 xs ys

groupElems :: Eq a => [a] -> [[a]]
groupElems [] = []
groupElems list = loop list []
  where
    loop :: Eq a => [a] -> [[a]] -> [[a]]
    loop [] acc = reverse acc
    loop (x : xs) [] = loop xs [[x]]
    loop (x : xs) acc = if x == headOfHead acc then loop xs ((x : head acc) : tail acc) else loop xs ([x] : acc)
      where
        headOfHead = head . head

readDigits :: String -> (String, String)
readDigits = span isDigit

filterDisj :: (a -> Bool) -> (a -> Bool) -> [a] -> [a]
filterDisj _ _ [] = []
filterDisj p1 p2 (x : xs)
  | p1 x || p2 x = x : filterDisj p1 p2 xs
  | otherwise = filterDisj p1 p2 xs

qsort :: Ord a => [a] -> [a]
qsort [] = []
qsort (x: xs) = qsort (filter (< x) xs) ++ (x : qsort (filter (>= x) xs))

squares'n'cubes :: Num a => [a] -> [a]
squares'n'cubes = concatMap (\x -> [x^2, x^3])

delAllUpper :: String -> String
delAllUpper [] = []
delAllUpper str = unwords (filter (any isLower) (words str))

max3 :: Ord a => [a] -> [a] -> [a] -> [a]
max3 = zipWith3 (\x y z -> max (max x y) z)