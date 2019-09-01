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

--fold
soma [] = 0
soma (x:xs) = x + (soma xs)


--representing a matrix
m = [[1,2,3],[4,5,6],[7,8,9],[0,0,-1]]

--transpose a matrix
transpose ([]:_) = []
transpose l = (map head l) : transpose (map tail l) 


