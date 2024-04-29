module ReaderToState where

import Control.Monad.State (State, state)
import Control.Monad.Reader (Reader, runReader)
import Control.Monad.Writer (Writer, runWriter)

readerToState :: Reader r a -> State r a
readerToState m = state (\s -> (runReader m s, s))

writerToState :: Monoid w => Writer w a -> State w a
writerToState m = case runWriter m of
  (a, w) -> state (\s -> (a, mappend s w))