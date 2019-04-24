data Tree = Node Int Tree Tree | Nil deriving Show

insert :: Tree -> [Int] -> Tree
insert t [] = t
insert Nil (x:xs) = insert (Node x Nil Nil) xs
insert (Node a l r) (x:xs)
  | x < a = insert (Node a (insert l [x]) r) xs
  | otherwise = insert (Node a l (insert r [x])) xs

------------------------------------------------------------------------

tree_equal :: Tree -> Tree -> Bool
tree_equal Nil Nil = True
tree_equal Nil (Node _ _ _) = False
tree_equal (Node _ _ _) Nil = False
tree_equal (Node a1 l1 r1) (Node a2 l2 r2) =
  (a1 == a2) && (tree_equal l1 l2) && (tree_equal r1 r2)
