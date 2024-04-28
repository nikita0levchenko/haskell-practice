module MapLike where

import Prelude hiding (lookup)
import qualified Data.List as L

class MapLike m where
    empty :: m k v
    lookup :: Ord k => k -> m k v -> Maybe v
    insert :: Ord k => k -> v -> m k v -> m k v
    delete :: Ord k => k -> m k v -> m k v
    fromList :: Ord k => [(k,v)] -> m k v
    fromList [] = empty
    fromList ((k,v):xs) = insert k v (fromList xs)

newtype ListMap k v = ListMap { getListMap :: [(k,v)] }
    deriving (Eq,Show)
    
instance MapLike ListMap where
  empty = ListMap []
  lookup _ (ListMap []) = Nothing
  lookup key (ListMap ((k,v) : xs)) | key == k = Just v
                          | otherwise = lookup key (ListMap xs)
                          
  delete key (ListMap list) = ListMap (deleteLoop list) where
    deleteLoop [] = []
    deleteLoop ((k,v) : xs) | key == k = xs
                            | otherwise = (k,v) : deleteLoop xs
                            
  insert key value (ListMap []) = ListMap [(key, value)]
  insert key value list = addToDeleted key value (delete key list) where
    addToDeleted k v (ListMap l) = ListMap ((k,v) : l)