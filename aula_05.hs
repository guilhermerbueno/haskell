--Currying and point free style
a = max 4

greater5 = (>5)

--function that receive function as parameter
zipWith' f [] _ = []
zipWith' f _ [] = []
zipWith' f (a:as) (b:bs) = f a b : (zipWith' f as bs)

--map and filter
map' :: (a->b) -> [a] -> [b]
map' f [] = []
map' f (a:as) = f a : (map' f as)

filter' :: (a -> Bool) -> [a] -> [a]
filter' _ [] = []
filter' p (x:xs)
 | p x = x :filter' p xs
 | otherwise = filter' p xs

impar x = x `mod` 2 == 1
