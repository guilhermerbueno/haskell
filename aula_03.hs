--tamanho lista
size [] = 0
size (a:as) = 1+size as 

--lista pares
listaPares y = [x|x<-[1..y], mod x 2 ==0]

--soma pares com list comprehension
soma [] = 0
soma (a:as) = a+soma as
somapares xs = soma [x|x <- xs, mod x 2 ==0]

--remove all
removeAll c lista = [y | y <- lista, y/= c]

--troca todos
trocaAll :: Eq a => a -> a -> [a] -> [a]
trocaAll velho novo lista = [if y == velho then novo else y | y <- lista]

--quicksort
qs [] = []
qs (x:xs) = qs menores ++ [x] ++ qs maiores
 where menores = [y | y <- xs, y<=x]
       maiores = [y | y <- xs, y>x]

--verifica se uma lista esta ordenada
ordenada :: Ord a => [a] -> Bool
ordenada [] = True
ordenada [x] = True
ordenada (a:b:xs)
 | a <= b = ordenada (b:xs)
 | otherwise = False

--troca uma vez
troca1 :: Eq a => a->a->[a]->[a]
troca1 _ _ [] = []
troca1 n v (a:as)
 | a == v = [n] ++ (troca1 n v as)
 | otherwise = [a] ++ (troca1 n v as) 

--verifica se a lista contem o elemento
contains x [] = False
contains x (a:as) = x == a || contains x as


--posicoes do elemento
posicoes :: Eq a => Num x => a -> [a] -> [x]
posicoes _ [] = []
posicoes x (a:as) = posicoes' x (a:as) 0
posicoes' _ [] _ = []
posicoes' x (a:as) pos
 | x == a = [pos] ++ posicoes' x as (pos+1)
 | otherwise = posicoes' x as (pos+1)

--split
split l x = splitAt (head (posicoes x l)) l

--drop n
dropX _ [] = []
dropX x lista = [x|x<-snd (splitAt x lista)]

--take
takeX _ [] = []
takeX x lista = [x|x<-fst (splitAt x lista)]
