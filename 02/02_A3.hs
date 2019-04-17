data Tree = Node String [Tree] deriving Show

t = Node "Wurzel" [
  Node "l" [],
  Node "m" [
    Node "lu" [],
    Node "ru" []
  ],
  Node "r" []
]

level :: Int -> Tree -> [String]
level 0 (Node x _) = [x]
level n (Node _ ls) = concatMap (level (n-1)) ls
  where
    concatMap :: (Tree -> [String]) -> [Tree] -> [String]
    concatMap _ [] = []
    concatMap f (x:xs) = f x ++ concatMap f xs
