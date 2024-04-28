module IOMonad where
  
import Data.List (isInfixOf, filter)
import Control.Monad (liftM)
import System.Directory (getDirectoryContents, removeFile)

-- type FilePath = String

main' :: IO ()
main' = do
  putStr "Substring: "
  pattern <- getLine
  if null pattern 
    then putStrLn "Canceled"
    else (getFiles pattern) >>= (mapM_  deleteFile)  -- (>>=) :: m a -> (a -> m b) -> m b
      

getFiles :: String -> IO [FilePath]
getFiles pattern =  liftM (filter (\x -> isInfixOf pattern x)) (getDirectoryContents ".")
  
deleteFile :: FilePath -> IO ()
deleteFile file = do
  putStrLn ("Removing file: " ++ file)
  removeFile file
  


