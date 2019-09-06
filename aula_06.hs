import Data.Char (toLower, toUpper)

--verifica se char eh vogal
ehVogal x = toLower(x) `elem` "aeiou"

--conta elementos em uma lista
--exemplo: contaL 'A' "abcdneuf"
contaL it l = foldl (\ac x -> if toLower(x) == toLower(it) then ac+1 else ac) 0 l
contaR it l = foldr (\x res -> if x == it then res+1 else res) 0 l

--conta quantos elementos satisfazem uma funcao
--exemplo: contafunc (>3) [1,2,3,4,5,6]
contafunc f l = foldl (\ac x -> if f x then ac+1 else ac) 0 l

--troca elemento velho por novo
troca velho novo l = foldr (\x res -> if x==velho then novo:res else x:res) [] l 
trocal velho novo l = foldl (\ac x -> if x==velho then ac++[novo] else ac++[x]) [] l

tuplas = [('a',0),('e',0),('i',0),('o',0),('u',0)]

--retorna o menor valor de uma lista
minimo l = foldr1 min l

--outra versao para contar elementos em uma lista
--exemplo: countOfElem 'x' "apsomcxapodx"
countOfElem elem = length . filter (==elem)

--soma1 :: (Eq a) => a -> [(a,Int)] -> [(a,Int)]
soma1 it l = [if it == a then b+1 else b | (a,b) <- l]

soma l = foldr (+) 0 l


countletters l = [(x,c) | x<-['A'..'z'], let c = (length.filter (==x)) l, c>0, ehVogal x ]

--vogalmaiscomum l = foldr (max fst) 0 l

