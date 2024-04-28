module Nat where

data Nat = Zero | Suc Nat deriving (Show, Eq)

fromNat :: Nat -> Integer
fromNat Zero = 0
fromNat (Suc n) = fromNat n + 1

toNat :: Integer -> Nat
toNat n | n == 0 = Zero
        | otherwise = Suc (toNat (n - 1))

add :: Nat -> Nat -> Nat
add x y = toNat(fromNat x + fromNat y)

mul :: Nat -> Nat -> Nat
mul x y = toNat(fromNat x * fromNat y)

fac :: Nat -> Nat
fac nat = toNat (factorial (fromNat nat) 1) where
  factorial 0 acc = acc
  factorial n acc = factorial (n - 1) (acc * n)

test1 = fromNat (Suc (Suc (Suc Zero))) == 3
test2 = toNat 3 == Suc (Suc (Suc Zero))
test3 = add (Suc (Suc (Suc Zero))) (Suc (Suc Zero)) == Suc (Suc (Suc (Suc (Suc Zero))))
test4 = mul (Suc (Suc (Suc Zero))) (Suc (Suc Zero)) == Suc (Suc (Suc (Suc (Suc (Suc Zero)))))
test5 = fac (Suc (Suc (Suc Zero))) == Suc (Suc (Suc (Suc (Suc (Suc Zero)))))