module XorMonoid where

newtype Xor = Xor { getXor :: Bool }
    deriving (Eq,Show)

instance Semigroup Xor where
  (Xor True) <> (Xor False) = Xor True
  (Xor False) <> (Xor True) = Xor True
  _ <> _ = Xor False

instance Monoid Xor where
    mempty = Xor False
    mappend (Xor True) (Xor False) = Xor True
    mappend (Xor False) (Xor True) = Xor True
    mappend _ _ = Xor False    