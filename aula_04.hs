--criando tipos
data Ponto = Ponto Float Float deriving (Eq, Show, Read)
data Figura = Circulo Ponto Float | Retangulo Ponto Ponto
area (Circulo _ r) = 2*3.14*r -- area (Circulo (Ponto 0 0) 3)
data Tree a = Vazia | No a (Tree a) (Tree a) deriving (Eq,Show,Read)

--calcula distancia entre pontos -> distancia (Ponto 0 0) (Ponto 1 2)
distancia (Ponto x y) (Ponto a b) = sqrt (dx**2 + dy**2)
        where  dx = x - a
               dy = y - b

--soma
soma Vazia = 0
soma (No x ae ad) = soma ae + soma ad + x

--verifica se é arvore de busca binaria
isAbb :: Ord a => Tree a -> Bool  -- nao colocar a resticao de Ord na definicao do tipo!!
isAbb Vazia  = True
isAbb (No _ Vazia Vazia)  = True
isAbb (No x Vazia ad) = isAbb ad && x < menor ad
isAbb (No x ae Vazia) = isAbb ae && x > maior ae
isAbb (No x ae ad) = isAbb ae && isAbb ad && x < menor ad && x > maior ae

maior (No x _ Vazia) = x
maior (No x _ ad) = maior ad

menor (No x Vazia _) = x
menor (No x ae _) = menor ae

--insere no vazio na arvore
singleton :: a -> Tree a  
singleton x = No x Vazia Vazia

--declarando uma arvore binaria
t = No 10 (No 5 (No 3 Vazia Vazia) (No 8 Vazia Vazia)) (No 15 (No 12 Vazia Vazia) (No 18 Vazia Vazia))
notT = No 10 (No 5 (No 3 Vazia Vazia) (No 8 Vazia Vazia)) (No 15 (No 12 Vazia Vazia) (No 2 Vazia Vazia))

--procurando um elemento na arvore
find Vazia _ = False
find (No x ae ad) y = if x == y
 then True
 else (find ae y) || (find ad y)

--insere item na arvore de busca binaria
insereNo y Vazia = singleton y
insereNo y (No x ae ad)
 | x == y = No x ae ad
 | y < x = No x (insereNo y ae) ad
 | y > x = No x ae (insereNo y ad)

--remove um no da arvore
--removeNo _ Vazia = Vazia
--removeNo y (No x ae ad)
-- | x == y =

--conta nos da arvore
contaNos Vazia = 0
contaNos (No x Vazia ad) = 1 + (contaNos ad)
contaNos (No x ae Vazia) = 1 + (contaNos ae)
contaNos (No x ae ad) = 1 + (contaNos ae) + (contaNos ad)

--profundidade da arvore
profundidade Vazia = 0
profundidade (No x Vazia ad) = 1 + (profundidade ad)
profundidade (No x ae Vazia) = 1 + (profundidade ae)
profundidade (No x ae ad)
 | (profundidade ae) > (profundidade ad) = 1 + profundidade ae
 | otherwise = 1+ profundidade ad
