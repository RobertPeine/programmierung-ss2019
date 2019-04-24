import Prelude hiding (even)

data Tree = Node Int [Tree] deriving Show

noLeaves :: Tree -> Int
noLeaves (Node _ []) = 1
noLeaves (Node _ ls) = f ls
  where
    f [] = 0
    f (x:xs) = noLeaves x + f xs

------------------------------------------------------------------------

even :: Tree -> Bool
even (Node a l) = length l `mod` 2 == 0 && f l
  where
    f [] = True
    f (x:xs) = even x && f xs
