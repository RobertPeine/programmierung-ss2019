import Prelude hiding (unzip)

unzip :: [(a, b)] -> ([a], [b])
unzip ls = (f ls, g ls)
  where
    f [] = []
    f ((a,_):ls) = a : f ls
    
    g [] = []
    g ((_,b):ls) = b : g ls

-- oder:

unzip' :: [(a, b)] -> ([a], [b])
unzip' ls = f [] [] ls
  where
    f x y [] = (x, y)
    f x y ((a,b):ls) = f (x ++ [a]) (y ++ [b]) ls 

-- oder:

unzip'' :: [(a, b)] -> ([a], [b])
unzip'' [] = ([], [])
unzip'' ((x,y):ls) = (x:xs, y:ys)
  where
    (xs, ys) = unzip'' ls
