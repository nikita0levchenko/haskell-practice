module Type where

type MyString = String

-- newtype should have one constructor with one parameter
newtype IntList = IList [Int] deriving Show

-- newtype is more la`y than data
