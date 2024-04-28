module MyMaybe where

import Data.Char
import Data.Maybe


{-
class Monad m where
  return :: a -> m a
  (>>=) :: m a -> (a -> m b) -> m b
  (>>) :: m a -> m b -> m b
  fail :: String -> m a

  import Prelude hiding (Maybe, Just, Nothing)

  data Maybe a = Nothing | Just a deriving (Show, Eq, Ord)

  instance Monad Maybe where
    return value = Just value
    (>>=) (Just value) f = f value
    (>>=) Nothing _ = Nothing
    (>>) (Just _) mb = mb
    (>>) Nothing _ = Nothing
-}

data Token = Number Int | Plus | Minus | LeftBrace | RightBrace deriving (Eq, Show)

asToken :: String -> Maybe Token
asToken "+" = Just Plus
asToken "-" = Just Minus
asToken "(" = Just LeftBrace
asToken ")" = Just RightBrace
asToken str = if all isDigit str then Just (Number (read str :: Int)) else Nothing

tokenize :: String -> Maybe [Token]
tokenize string = if Nothing `elem` mappedWords then Nothing else toMonadList mappedWords where
  mappedWords = map asToken (words string)
  toMonadList [] = Just []
  toMonadList (x : xs) = do
    x' <- x
    xs' <- toMonadList xs
    Just(x' : xs') 

test1 = tokenize "1 + 2" == Just [Number 1,Plus,Number 2]
test2 = tokenize "1 + ( 7 - 2 )" == Just [Number 1,Plus,LeftBrace,Number 7,Minus,Number 2,RightBrace]
test3 = isNothing (tokenize "1 + abc")

testAll = test1 && test2 && test3

