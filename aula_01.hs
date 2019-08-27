--tamanho de uma lista
tamanho [] = 0
tamanho (a:as) = 1 + tamanho as

--soma dos elementos de uma lista
soma [] = 0
soma (a:as) = a + soma as

--soma dos elementos pares
somaPares [] = 0
somaPares (a:as) = if mod a 2 == 0
 then a + (somaPares as)
 else somaPares as

--retorna o ultimo elemento da lista
retornaUlt (a:as) = if as == []
 then a
 else retornaUlt as

--existe elemento x na lista
existe [] _ = False
existe (a:as) x = if a == x
 then True
 else existe as x

--qual o maior valor
maior a b = if a>b
 then True
 else False

--gera lista de n até 1!!!!!!!!!!!!!!!!!!!!!!!
--geraLista a = if a > 1
-- then a : geraLista (a-1)
-- else a : 1

--qual posicao do elemento na lista
posicao [] _ = 0
posicao (a:as) x = if a == x
 then 1
 else 1 + posicao as x

--quantas vezes o elemento aparece na lista
conta [] _ = 0
conta (a:as) x = if a == x
 then 1 + conta as x
 else conta as x

soma2 l = soma2' l 0
  where soma2' [] acc = acc
        soma2' (x:xs) acc = soma2' xs (x+acc)

contaItem _ [] = 0
contaItem x l = contaItem' x l 0
 where contaItem' x [] acc = acc
       contaItem' x (a:as) acc = if x == a then contaItem' x as (acc+1) else contaItem' x as acc

--qual o valor maximo existente na lista
maxi [] = 0
maxi (a:as) = maximo (a:as) 0
maximo (a:[]) x = if a > x
 then a
 else x
maximo (a:as) x = if a > x
 then maximo as a
 else maximo as x

--reverter uma lista
reverter [] = []
reverter (a:as) = reverter as ++ [a]

--gera lista de 1 a n
gera x = gera' x 1 []
gera' x y l = if x == y
 then l++[y]
 else gera' x (y+1) l++[y]

--remove ultimo elemento
removeLast (a:as) = removeLast' (a:as) []
removeLast' (a:[]) l = l
removeLast' (a:as) l = removeLast' as l++[a]

--soma posicoes pares
somaPosPares [] = 0
somaPosPares (a:b:[]) = a+(somaPosPares [])
somaPosPares (a:[]) = a+somaPosPares []
somaPosPares (a:b:as) = a+(somaPosPares as)

--intercala listas


--remove 1 vez
--remove (x:xs) y = if x == y
-- then xs
-- else remove x:xs y

--intercala x y = intercala' x y []
-- where intercala [] y l = y:l
--	intercala x [] l = x:l
--	intercala' (x:xs) (y:ys) l = x:y:l
