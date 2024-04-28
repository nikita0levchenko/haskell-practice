module ParsePerson where

import Data.Char
import Data.List


data Error = ParsingError | IncompleteDataError | IncorrectDataError String deriving (Show, Eq)
data Person = Person { firstName :: String, lastName :: String, age :: Int } deriving (Show, Eq)

parsePerson :: String -> Either Error Person
parsePerson string = case checkAge (checkAllFieldsProvided (processedStringToPairs string)) of
  (Right list) -> case (lookup "firstName" list, lookup "lastName" list, lookup "age" list) of
    (Just fn, Just ln, Just a) -> Right Person{firstName = fn, lastName = ln, age = read a :: Int}
    _ -> Left IncompleteDataError
  (Left someErr) -> Left someErr   
    


checkAge :: Either Error [(String, String)] -> Either Error [(String, String)]
checkAge (Left someError) = Left someError
checkAge (Right list) = case lookup "age" list of
  (Just someAge) -> case find (not . isDigit) someAge of
    (Just _) -> Left (IncorrectDataError someAge)
    Nothing -> Right list
  Nothing -> Left IncompleteDataError  

checkAllFieldsProvided :: Either Error [(String, String)] -> Either Error [(String, String)]
checkAllFieldsProvided (Left someError) = Left someError
checkAllFieldsProvided (Right list) = if checkNecessaryFields then Right list else Left IncompleteDataError where
  checkNecessaryFields = case (lookup "firstName" list, lookup "lastName" list, lookup "age" list) of
    (Just _, Just _, Just _) -> True
    _                        -> False     

processedStringToPairs :: String -> Either Error [(String, String)]
processedStringToPairs str = if Left ParsingError `elem` pairList then Left ParsingError else Right mappedList where
  pairList = map splitIntoPair (lines str)
  mappedList = map (\(Right (key, value)) -> (key, value)) pairList
  

splitIntoPair :: String -> Either Error (String, String)
splitIntoPair str = case wordsWhen (== '=') str of
  [x, y] -> Right (trim x, trim y)
  _ -> Left ParsingError

wordsWhen     :: (Char -> Bool) -> String -> [String]
wordsWhen p s =  case dropWhile p s of
                      "" -> []
                      s' -> w : wordsWhen p s''
                            where (w, s'') = break p s'

trim :: [Char] -> String
trim xs = dropSpaceTail "" $ dropWhile isSpace xs

dropSpaceTail :: [Char] -> [Char] -> String
dropSpaceTail maybeStuff "" = ""
dropSpaceTail maybeStuff (x:xs)
        | isSpace x = dropSpaceTail (x:maybeStuff) xs
        | null maybeStuff = x : dropSpaceTail "" xs
        | otherwise       = reverse maybeStuff ++ x : dropSpaceTail "" xs
        
test1 = parsePerson "firstName = John\nlastName = Connor\nage = 30" == Right (Person {firstName = "John", lastName = "Connor", age = 30})
test2 = parsePerson "firstName = John Smith\nlastName = Connor\nage = 30\nasde=as11" == Right (Person {firstName = "John Smith", lastName = "Connor", age = 30})
test3 = parsePerson "firstName=Barbarian\nlastName=Conn On\nage=30" == Right (Person {firstName = "Barbarian", lastName = "Conn On", age = 30})
test4 = parsePerson "firstName=Barbarian\nlastName=Conn On\nage=30\ng dsfsd" == Left ParsingError        
test5 = parsePerson "firstName=Barbarian\nlastName=Conn On\nage=30\ng dsfsd\n drrr" == Left ParsingError        
test6 = parsePerson "firstName=Barbarian\nlastName=Conn On" == Left IncompleteDataError       
test7 = parsePerson " firstName = John\nlastName = Connor\nage = 2f8 " == Left (IncorrectDataError "2f8")
testAll = foldl (&&) True [test1, test2, test3, test4, test5, test6, test7]