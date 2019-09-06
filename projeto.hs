--1. Ler o arquivo
--1.1 Salvar os pontos e suas coordenadas (até encontrar linha em branco). Ex.: pontos = [('a',[x1,x2,x3]),('b',[y1,y2,3]),('c',[z1,z2,z3],...)
--1.2 Ler os pontos rotulados. Ex: [('a',1),('b',3),...,('p',1)]
--1.3 Separar os pontos não rotulados dos pontos rotulados
--2. Calcular a distancia euclidiana entre um ponto e uma lista de pontos (usar applicative pode ser uma boa ideia), esse metodo retornar o ponto mais proximo. Ex: distanciaEuclidiana p pontos
--2.1 atualizar a lista de pontos rotulados com o ponto mais proximo encontrado acima
import System.IO
import Data.Char

--1.3 - dada uma lista de pontos, verifica e retorna somente aqueles que pertencem a lista de pontos com rotulos (ou os que nao pertencem)
pontosRotulados pts rotulos = filter (\pt -> (fst pt) `elem` (map fst rotulos)) pts
pontosNaoRotulados pts rotulos = filter (\pt -> (fst pt) `notElem` (map fst rotulos)) pts

--2 - calcula a distancia euclidiana entre 2 pontos dados
distanciaEuclidiana pt1 pt2 = sqrt (foldl (+) 0 $ [a*a | a <- (zipWith (-) pt1 pt2)])

--inverte a posicao dos elementos das tuplas de uma lista [(a,b)] = [(b,a)]
inverterTuplas l = (map (uncurry $ flip (,)) l)

converterParaNum (a,b) = (a, map fromInteger ([read c::Integer | c <-b]))

processFile f =
 let allLines = lines f
     pontos = map (\line -> (head (words line), tail (words line))) (takeWhile (not . null) allLines)
     rotulos = map (\line -> (head (words line), tail (words line))) (takeWhile (not . null) (reverse allLines))
     rotulados = pontosRotulados pontos rotulos
     naoRotulados = pontosNaoRotulados pontos rotulos
     rotuladosInvertido = inverterTuplas (map converterParaNum rotulados) --convertendo os valores lidos do arquivo para o tipo Num para poder fazer as contas
     naoRotuladosInvertido = inverterTuplas (map converterParaNum naoRotulados)
     distancias = distanciaEuclidiana <$> (map fst rotuladosInvertido) <*> (map fst naoRotuladosInvertido)
     --linhasNaoNulas = filter (\line -> null line == False) allLines
     --pontos = map (\line -> (head (words line), tail (words line))) linhasNaoNulas
    -- result = unlines pontos
 in distancias

main = do
 file <- getContents
 let pontos = processFile file
 print pontos
-- [(a,b) | (a,b) <- pontos]
-- putStrLn unwords map fst pontos
-- writeFile "output.txt" [a | ([a],b) <- pontos]
-- writeFile "output.txt" [a | [a] <- pontos]

