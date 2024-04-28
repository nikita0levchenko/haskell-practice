module FunctorTypeClass where

data Point3D a = Point3D a a a deriving Show

instance Functor Point3D where
  fmap f (Point3D x y z) = Point3D (f x) (f y) (f z)

data GeomPrimitive a = Point (Point3D a) | LineSegment (Point3D a) (Point3D a)

instance Functor GeomPrimitive where
  fmap f (Point point) = Point (fmap f point)
  fmap f (LineSegment point1 point2) = LineSegment (fmap f point1) (fmap f point2)


data Tree a = Leaf (Maybe a) | Branch (Tree a) (Maybe a) (Tree a) deriving Show

instance Functor Tree where
  fmap _ (Leaf Nothing) = Leaf Nothing
  fmap f (Leaf just) = Leaf (fmap f just)
  fmap f (Branch l j r) = Branch (fmap f l) (fmap f j) (fmap f r)

data Entry k1 k2 v = Entry (k1, k2) v  deriving Show
data Map k1 k2 v = Map [Entry k1 k2 v]  deriving Show

instance Functor (Entry k1 k2 ) where
  fmap f (Entry (k1, k2) v) = Entry (k1, k2) (f v)
  
instance Functor (Map k1 k2) where
  fmap f (Map list) = Map( map (fmap f) list)
  
{-
Functor laaws:
1) fmap id xs == id xs
2) fmap (g . f) xs = (fmap g . fmap f) xs
-}  