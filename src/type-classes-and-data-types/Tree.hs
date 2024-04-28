module Tree where

data Tree a = Leaf a | Node (Tree a) (Tree a) deriving (Show, Eq)

height :: Tree a -> Int
height (Leaf _) = 0
height (Node left right) = 1 + max (height left) (height right)

size :: Tree a -> Int
size (Leaf _) = 1
size (Node left right) = 1 + size left + size right

avg :: Tree Int -> Int
avg t =
    let (c,s) = go t
    in s `div` c
  where
    go :: Tree Int -> (Int,Int)
    go tree = foldl (\(x1, y1) (x2, y2) -> (x1 + x2, y1 + y2)) (0,0) (treeLoop tree) where
      treeLoop (Leaf a) = [(1, a)]
      treeLoop (Node left right) = treeLoop left ++ treeLoop right 
      

{- Tests

   Tree t1 =    []
              /    \
             /      \
          []          []
        /    \      /    \
       /      \    /      \
      1        2  3        4             => height t == 3
      
      
   Tree t2 =          []
                /            \
               /              \
             []                []
           /    \            /    \
          /      \          /      \
        []        []       []       []             => height t == 3   
      /    \    /    \    /  \     /  \
     /      \  /      \  /    \   /    \
    1       2 3       4  5    6  7      8
-}

t1 = Node (Node (Leaf (1 :: Int)) (Leaf (2 :: Int))) (Node (Leaf (3 :: Int)) (Leaf (4 :: Int)))
t2 = Node (Node (Node (Leaf 1) (Leaf 2)) (Node (Leaf 3) (Leaf 4))) (Node (Node (Leaf 5) (Leaf 6)) (Node (Leaf 7) (Leaf 8)))
test1 = height t1 == 2
test2 = height t2 == 3
test3 = size t1 == 7
test4 = size t2 == 15
