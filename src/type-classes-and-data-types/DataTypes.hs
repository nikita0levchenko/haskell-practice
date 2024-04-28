module DataTypes where
import Data.Maybe
import Data.Function
import Data.Char
import Data.List
import Data.Complex
import Data.Ratio



-- sum data type
data B = T | F deriving Show

notB :: B -> B
notB T = F
notB F = T

charToInt :: Char -> Int
charToInt ch = fromJust (lookup ch (zip ['0'..'9'] [0..9]))

data LogLevel = Error | Warning | Info

cmp :: LogLevel -> LogLevel -> Ordering
cmp log1 log2 = compare (ord log1) (ord log2) where
  ord Error = 3
  ord Warning = 2
  ord Info = 1

-- case of structure

lessThanError :: LogLevel -> Bool
lessThanError lvl = case cmp lvl Error of
  LT -> True
  _ -> False

-- production data type
data Point = Pt Double Double deriving Show

origin :: Point
origin = Pt 0.0 0.0

distanceToOrigin :: Point -> Double
distanceToOrigin (Pt x y) = sqrt (x^2 + y^2)

distance :: Point -> Point -> Double
distance (Pt x1 y1) (Pt x2 y2) = sqrt ((x1 - x2)^2 + (y1 - y2)^2)

data Bit = Zero | One deriving (Eq, Show)
data Sign = Minus | Plus deriving (Eq, Show)
data Z = Z Sign [Bit] deriving (Eq,Show)

add :: Z -> Z -> Z
add z1 (Z _ []) = z1
add (Z _ []) z2 = z2
add z1 z2 = intToZ (zToInt z1 + zToInt z2)

mul :: Z -> Z -> Z
mul _ (Z _ []) = Z Plus []
mul (Z _ []) _ = Z Plus []
mul z1 z2 = intToZ (zToInt z1 * zToInt z2)


zToInt :: Z -> Int
zToInt (Z sign bits) | sign == Minus = (-1) * bitToInt (zip bits [0..])
                     | otherwise = bitToInt (zip bits [0..])

bitToInt :: [(Bit, Int)] -> Int
bitToInt [] = 0
bitToInt ((x, indx) : xs) | x == Zero = bitToInt xs
                        | x == One = (2^indx) + bitToInt xs

intToZ :: Int -> Z
intToZ int | int < 0 = Z Minus (intToBit ((-1) * int))
           | int == 0 = Z Plus []
           | otherwise = Z Plus (intToBit int)

intToBit :: Int -> [Bit]
intToBit digit | div digit 2 == 0 =  [matchBit (mod digit 2)]
               | otherwise = matchBit(mod digit 2) : intToBit (div digit 2) where
                 matchBit :: Int -> Bit
                 matchBit int | int == 0 = Zero
                              | otherwise = One

test001 = (add (Z Plus []) (Z Plus [])) == Z Plus []
test002 = (add (Z Plus []) (Z Plus [One])) == Z Plus [One]
test003 = (add (Z Plus []) (Z Minus [One])) == Z Minus [One]

test011 = (add (Z Plus [Zero, One, One]) (Z Plus [One])) == Z Plus [One, One, One]
test012 = (add (Z Plus [Zero, One, One]) (Z Plus [Zero, One])) == Z Plus [Zero, Zero, Zero, One]
test013 = (add (Z Plus [Zero, One, One]) (Z Plus [Zero, One, One])) == Z Plus [Zero, Zero, One, One]

test021 = (add (Z Minus [Zero, One, One]) (Z Minus [One])) == Z Minus [One, One, One]
test022 = (add (Z Minus [Zero, One, One]) (Z Minus [Zero, One])) == Z Minus [Zero, Zero, Zero, One]
test023 = (add (Z Minus [Zero, One, One]) (Z Minus [Zero, One, One])) == Z Minus [Zero, Zero, One, One]

test031 = (add (Z Minus [Zero, One, One]) (Z Plus [One])) == Z Minus [One, Zero, One]
test032 = (add (Z Minus [Zero, One, One]) (Z Plus [Zero, One])) == Z Minus [Zero, Zero, One]
test033 = (add (Z Minus [Zero, One, One]) (Z Plus [Zero, One, One])) == Z Plus []

test041 = (add (Z Plus [Zero, One, One]) (Z Minus [One])) == Z Plus [One, Zero, One]
test042 = (add (Z Plus [Zero, One, One]) (Z Minus [Zero, One])) == Z Plus [Zero, Zero, One]
test043 = (add (Z Plus [Zero, One, One]) (Z Minus [Zero, One, One])) == Z Plus []

test051 = (add (Z Plus [One]) (Z Minus [One])) == Z Plus []
test052 = (add (Z Plus [One]) (Z Minus [One, One])) == Z Minus [Zero, One]
test053 = (add (Z Plus [One]) (Z Minus [Zero, One])) == Z Minus [One]
test054 = (add (Z Plus [One]) (Z Minus [Zero, Zero, Zero, One])) == Z Minus [One, One, One]
test055 = (add (Z Plus [One]) (Z Minus [Zero, One, Zero, One])) == Z Minus [One, Zero, Zero, One]
test056 = (add (Z Plus [Zero, One]) (Z Minus [Zero, One, One])) == Z Minus [Zero, Zero, One]
test057 = (add (Z Plus [Zero, One]) (Z Minus [Zero, Zero, One])) == Z Minus [Zero, One]
test058 = (add (Z Plus [One, Zero, One]) (Z Minus [Zero, One, Zero, One])) == Z Minus [One, Zero, One]

test101 = (mul (Z Plus []) (Z Plus [])) == Z Plus []
test102 = (mul (Z Plus []) (Z Plus [One])) == Z Plus []
test103 = (mul (Z Plus []) (Z Minus [One])) == Z Plus []
test104 = (mul (Z Plus [One]) (Z Plus [])) == Z Plus []
test105 = (mul (Z Minus [One]) (Z Plus [])) == Z Plus []

test111 = (mul (Z Plus [One]) (Z Plus [One])) == Z Plus [One]
test112 = (mul (Z Minus [One]) (Z Plus [One])) == Z Minus [One]
test113 = (mul (Z Plus [One]) (Z Minus [One])) == Z Minus [One]
test114 = (mul (Z Minus [One]) (Z Minus [One])) == Z Plus [One]

test121 = (mul (Z Plus [One]) (Z Plus [Zero, One])) == Z Plus [Zero, One]
test122 = (mul (Z Plus [Zero, Zero, One]) (Z Plus [Zero, Zero, One])) == Z Plus [Zero, Zero, Zero, Zero, One]

test131 = (mul (Z Plus [One, Zero, One, Zero, One]) (Z Plus [One, One, One])) == Z Plus [One, One, Zero, Zero, One, Zero, Zero, One]

testAdd = test001 && test002 && test003 && test011 && test012 && test013 && test021 && test022 && test023 && test031 && test032 && test033 && test041 && test042 && test043 && test051 && test052 && test053 && test054 && test055 && test056 && test057 && test058
testMul = test101 && test102 && test103 && test104 && test105 && test111 && test112 && test113 && test114 && test121 && test122 && test131

testAll = testAdd && testMul

{- lazy patterns
lazy patterns is useful when we want to create irrefutable pattern
-}

myFromMaybe (Just x) = x
myFromMaybe Nothing = error "!!!"

myFromMaybe' ~(Just x) = x
myFromMaybe' Nothing = error "!!!" -- this branch is never will be reached

-- OOP like things. Fro example this is class Person(name: String, lastName: String, age: Int)

data Person' = Person' String String Int

firstName' :: Person' -> String
firstName' (Person' name _ _) = name

lastName' :: Person' -> String
lastName' (Person' _ lastName _) = lastName

age' :: Person' -> Int
age' (Person' _ _ age) = age

-- You can rewrite things above in more short way:

data Person = Person {firstName :: String, lastName :: String, age :: Int} deriving (Show, Eq)

{- In OOP you can write john.age to get age. In haskell you will do this: john & age
    & operator located in Data.Function module

    You can create instance of data structure like this
    let xavier = Person{ age = 40, lastName = "Xavier", name = "Charlse"}
    Pay attention on fields order in constructor above

    You also can copy instance of data structure like this:
-}

copyAgeLikeThis :: Int -> Person -> Person
copyAgeLikeThis newAge person = person {age = newAge}

updateLastName :: Person -> Person -> Person
updateLastName (Person _ p1LastName _) p2 = p2 {lastName = p1LastName}

infixl 1 &
x & f = f x

abbrFirstName :: Person -> Person
abbrFirstName person | length (firstName person) < 2 = person
                     | otherwise = person{firstName = take 2 (firstName person)}

-- Type parameters

data Coord a = Coord a a deriving Show

distance1 :: Coord Double -> Coord Double -> Double
distance1 (Coord x1 y1) (Coord x2 y2) = sqrt ((x1 - x2)^2 + (y1 - y2)^2)

manhDistance :: Coord Int -> Coord Int -> Int
manhDistance (Coord x1 y1) (Coord x2 y2) = abs (x1 - x2) + abs (y1 - y2)

getCenter :: Double -> Coord Int -> Coord Double
getCenter size (Coord x y) = Coord (size * fromIntegral x + center) (size * fromIntegral y + center) where
  center = size / 2

getCell :: Double -> Coord Double -> Coord Int
getCell size (Coord x y) = (Coord xCoord yCoord) where
  xCoord = floor (x / size)
  yCoord = floor (y / size)

-- You can enforce computation of constructors parameters with !

data ActiveCoord a = ActiveCoord !a !a -- ! means what when structure will be created - parameters will be computed

-- create complex digits and dario digits

complexDigit = 2 :+ 5
ratioDigit = 2 % 3


-- recursive data type

data List a = Nil | Cons a (List a) deriving Show

fromList :: List a -> [a]
fromList Nil = []
fromList (Cons x xs) = x : fromList xs

toList :: [a] -> List a
toList [] = Nil
toList (x : xs) = Cons x (toList xs)

