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
distanciaEuclidiana (label1, pt1) (label2, pt2) = (sqrt (foldl (+) 0 $ [a*a | a <- (zipWith (-) pt1 pt2)]), label1, label2)

--inverte a posicao dos elementos das tuplas de uma lista [(a,b)] = [(b,a)]
inverterTuplas l = (map (uncurry $ flip (,)) l)

--converte as coordenadas dos pontos lidos do arquivo de String para Num
converterParaNum (a,b) = (a, map fromInteger ([read c::Integer | c <-b]))

--recebe a distancia calculada, a label do ponto que ja pertence a uma classe e a label do ponto que desejamos rotular
rotulaPonto (dist, rotulado, rotular) rotulos = (rotular, snd (head (filter (\rot -> fst rot == rotulado) rotulos)))

adicionaSaida rot rotulos = [(b, [a]:(fst rot)) | (a,b) <- rotulos, b == (snd rot)]

formataSaida [] l = l
formataSaida (a:as) l = if (snd a) `elem` map snd l
 then formataSaida as ([if classe == (snd a) then (label ++ (fst a), classe) else (label, classe) | (label, classe) <- l])--(filter (\rot -> (fst rot) == (fst a)) l)
 else formataSaida as (l++[a])

processFile f =
 let allLines = lines f
     --lendo o arquivo
     pontos = map (\line -> (head (words line), tail (words line))) (takeWhile (not . null) allLines)
     rotulos = map (\line -> (head (words line), head (tail (words line)))) (takeWhile (not . null) (reverse allLines))

     --separando os pontos lidos em rotulados e nao rotulados
     rotulados = pontosRotulados pontos rotulos
     naoRotulados = pontosNaoRotulados pontos rotulos

     --convertendo os pontos lidos de String para Num para poder calcular as distancias
     naoRotuladosNum = (map converterParaNum naoRotulados)
     rotuladosNum = (map converterParaNum rotulados)
     
     --calcula a distancia de todos os pontos rotulados para os que ainda nao estao rotulados e retorna uma lista de [(dist, labelRotulado, labelARotular)]
     distancias = distanciaEuclidiana <$> rotuladosNum <*> naoRotuladosNum
     distanciaMinima = minimum distancias
     rotulos2 = (rotulaPonto distanciaMinima rotulos):rotulos

     saida = inverterTuplas (formataSaida [([a],b) | (a,b) <- rotulos2] [])
 in saida

main = do
 file <- getContents
 let saida = processFile file
 --print (unlines [a ++ " " ++ unwords b | (a,b) <- saida])
 writeFile "output.txt" (unlines [a ++ " " ++ unwords b | (a,b) <- saida])
