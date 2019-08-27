maior a b
 | a > b = a
 | otherwise = b

posicao it [] = 0
posicao it (x:xs)
 | it == x = 1
 | otherwise = if (posicao it xs) == 0 then 0 else (posicao it xs) + 1

--maior com where
maior2 [x] = x 
maior2 (x:xs) = if x > mm then x else mm
 where mm = maior2 xs

--maior com acumulador
maior3 [x] = x
maior3 (x:xs) = let mm = maior3 xs
 in if x>mm then x else mm

--soma [] = 0
--soma (x:xs) = x + (soma xs)

soma l = soma' l 0
 where soma' [] acc = acc
       soma' (x:xs) acc = soma' xs (x+acc)

reverte l = reverte' l []
   where 
     reverte' [] acc = acc
     reverte' (x:xs) acc = reverte' xs (x:acc)


posicoes [] _ = 0
posicoes l y = posicoes' l y 1 []
 where 
  posicoes' (x:xs) y i v = if x == y
  then posicoes' xs y (i+1) v++[i]
  else v
