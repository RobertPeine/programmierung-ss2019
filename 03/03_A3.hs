f :: [Int] -> Int
f xs = foldr product 1 (map square (filter even' xs))
  where
    even' x = mod x 2 == 0
    square x = x * x
    product x y = x * y

-- Haskell-Funktionen nutzen:

f' :: [Int] -> Int
f' xs = foldr (*) 1 (map (^2) (filter even xs))

-- Funktionskomposition nutzen:

f'' :: [Int] -> Int
f'' = foldr (*) 1 . map (^2) . filter even

-- Ohne Nutzung von map:

f''' :: [Int] -> Int
f''' = foldr g 1 . filter even
  where
    g x y = x^2 * y

-- Ohne Nutzung von map und mit anonymer Funktion / Lamda-Funktion:

f'''' :: [Int] -> Int
f'''' = foldr (\x y -> x^2 * y) 1 . filter even
