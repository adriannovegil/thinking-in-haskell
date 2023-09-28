module OrdenSuperior where

import Test.QuickCheck

main :: IO ()
main = return ()

-- 6.1 - Segmento inicial verificando una propiedad
-- -----------------------------------------------------------------------------
-- Redefinir por recursión la función
--
-- takeWhile :: (a -> Bool) -> [a] -> [a]
--
-- tal que (takeWhile p xs) es la lista de los elementos de xs hasta el primero 
-- que no cumple la propiedad p. Por ejemplo,
--
-- takeWhile (<7) [2,3,9,4,5] == [2,3]

takeWhile' :: (a -> Bool) -> [a] -> [a]
takeWhile' _ [] = []
takeWhile' p (x:xs)
    | p x       = x : takeWhile' p xs
    | otherwise = []

-- 6.2 - Complementario del segmento inicial verificando una propiedad
-- -----------------------------------------------------------------------------
-- Redefinir por recursión la función
--
-- dropWhile :: (a -> Bool) -> [a] -> [a]
--
-- tal que (dropWhile p xs) es la lista obtenida eliminando los elementos de xs 
-- hasta el primero que cumple la propiedad p. Por ejemplo,
--
-- dropWhile (<7) [2,3,9,4,5] => [9,4,5]

dropWhile' :: (a -> Bool) -> [a] -> [a]
dropWhile' _ [] = []
dropWhile' p (x:xs)
    | p x       = dropWhile' p xs
    | otherwise = x:xs

-- 6.3 - Concatenación de una lista de listas
-- -----------------------------------------------------------------------------
-- 6.3.1 - Redefinir, por recursión, la función concat. Por ejemplo,
-- 
-- concatR [[1,3],[2,4,6],[1,9]] == [1,3,2,4,6,1,9]

concatR :: [[a]] -> [a]
concatR []       = []
concatR (xs:xss) = xs ++ concatR xss

-- 6.3.2 - Redefinir, usando foldr, la función concat. Por ejemplo,
--
-- concatP [[1,3],[2,4,6],[1,9]] == [1,3,2,4,6,1,9]

concatP :: [[a]] -> [a]
concatP = foldr (++) []

-- 6.4 - División de una lista numérica según su media
-- -----------------------------------------------------------------------------
-- 6.4.1 - La función
--
-- divideMedia :: [Double] -> ([Double],[Double])
--
-- dada una lista numérica, xs, calcula el par (ys,zs), donde ys contiene los 
-- elementos de xs estrictamente menores que la media, mientras que zs contiene 
-- los elementos de xs estrictamente mayores que la media. Por ejemplo,
--
-- divideMedia [6,7,2,8,6,3,4] == ([2.0,3.0,4.0],[6.0,7.0,8.0,6.0])
-- divideMedia [1,2,3] == ([1.0],[3.0])
--
-- Definir la función divideMedia por filtrado, comprensión y recursión
--
-- La definición por filtrado es

divideMediaF :: [Double] -> ([Double],[Double])
divideMediaF xs = (filter (<m) xs, filter (>m) xs)
    where m = media xs

-- donde (media xs) es la media de xs. Por ejemplo,
-- 
-- media [1,2,3] == 2.0
-- media [1,-2,3.5,4] == 1.625

media :: [Double] -> Double
media xs = (sum xs) / fromIntegral (length xs)

-- En la definición de media se usa la función fromIntegral tal que 
-- (fromIntegral x) es el número real correspondiente al número entero x.
-- La definición por comprensión es

divideMediaC :: [Double] -> ([Double],[Double])
divideMediaC xs = ([x | x <- xs, x < m], [x | x <- xs, x > m])
    where m = media xs

-- La definición por recursión es

divideMediaR :: [Double] -> ([Double],[Double])
divideMediaR xs = divideMediaR' xs
    where m = media xs
          divideMediaR' [] = ([],[])
          divideMediaR' (x:xs) | x < m = (x:ys, zs)
                             | x == m = (ys, zs)
                             | x > m = (ys, x:zs)
                             where (ys, zs) = divideMediaR' xs

-- 6.4.2 - Comprobar con QuickCheck que las tres definiciones anteriores 
-- divideMediaF, divideMediaC y divideMediaR son equivalentes.

prop_divideMedia :: [Double] -> Bool
prop_divideMedia xs =
    divideMediaC xs == d &&
    divideMediaR xs == d
    where d = divideMediaF xs

-- La comprobación es
-- 
-- ghci> quickCheck prop_divideMedia
-- +++ OK, passed 100 tests.
--
-- 6.4.3 - Comprobar con QuickCheck que si (ys,zs) es el par obtenido 
-- aplicándole la función divideMediaF a xs, entonces la suma de las longitudes
-- de ys y zs es menor o igual que la longitud de xs.

prop_longitudDivideMedia :: [Double] -> Bool
prop_longitudDivideMedia xs =
    length ys + length zs <= length xs
    where (ys,zs) = divideMediaF xs

-- La comprobación es
-- 
-- ghci> quickCheck prop_longitudDivideMedia
-- +++ OK, passed 100 tests.
--
-- 6.4.4 Comprobar con QuickCheck que si (ys,zs) es el par obtenido aplicándole
-- la función divideMediaF a xs, entonces todos los elementos de ys son menores 
-- que todos los elementos de zs.

prop_divideMediaMenores :: [Double] -> Bool
prop_divideMediaMenores xs =
    and [y < z | y <- ys, z <- zs]
    where (ys,zs) = divideMediaF xs

-- La comprobación es
-- 
-- ghci> quickCheck prop_divideMediaMenores
-- +++ OK, passed 100 tests.
--
-- 6.4.5 - Comprobar con QuickCheck que si (ys,zs) es el par obtenido 
-- aplicándole la función divideMediaF a xs, entonces la media de xs no 
-- pertenece a ys ni a zs.
-- Nota: Usar la función notElem tl que (notElem x ys) se verifica si y no 
-- pertenece a ys.

prop_divideMediaSinMedia :: [Double] -> Bool
prop_divideMediaSinMedia xs =
    notElem m (ys ++ zs)
    where m = media xs
          (ys,zs) = divideMediaF xs

-- La comprobación es
-- 
-- ghci> quickCheck prop_divideMediaSinMedia
-- +++ OK, passed 100 tests.
--
-- 6.5 - Segmentos cuyos elmentos verifican una propiedad
-- -----------------------------------------------------------------------------
-- Definir la función
-- 
-- segmentos :: (a -> Bool) -> [a] -> [a]
--
-- tal que (segmentos p xs) es la lista de los segmentos de xs cuyos elementos 
-- verifican la propiedad p. Por ejemplo,
--
-- segmentos even [1,2,0,4,5,6,48,7,2] == [[],[2,0,4],[6,48],[2]]

segmentos :: (a -> Bool) -> [a] -> [[a]]
segmentos _ [] = []
segmentos p xs =
    takeWhile p xs : (segmentos p (dropWhile (not.p) (dropWhile p xs)))

-- 6.6 - Listas con elementos consecutivos relacionados
-- -----------------------------------------------------------------------------
-- Definir la función
-- 
-- relacionados :: (a -> a -> Bool) -> [a] -> Bool
--
-- tal que (relacionados r xs) se verifica si para todo par (x,y) de elementos
-- consecutivos de xs se cumple la relación r. Por ejemplo,
--
-- relacionados (<) [2,3,7,9]               == True
-- relacionados (<) [2,3,1,9]               == False
-- relacionados equivalentes [3205,50,5014] == True

relacionados :: (a -> a -> Bool) -> [a] -> Bool
relacionados r (x:y:zs) = (r x y) && relacionados r (y:zs)
relacionados _ _ = True

-- Una definición alternativa es

relacionados' :: (a -> a -> Bool) -> [a] -> Bool
relacionados' r xs = and [r x y | (x,y) <- zip xs (tail xs)]

-- 6.7 - Agrupamiento de elmentos de una lista de listas
-- -----------------------------------------------------------------------------
-- Definir la función
--
-- agrupa :: Eq a => [[a]] -> [[a]]
--
-- tal que (agrupa xss) es la lista de las listas obtenidas agrupando lo 
-- sprimeros elementos, los segundos,... de forma que las longitudes de la 
-- lista del resultado sean iguales a la más corta de xss. Por ejemplo,
--
-- agrupa [[1..6],[7..9],[10..20]] == [[1,7,10],[2,8,11],[3,9,12]]
-- agrupa []                       == []

agrupa :: Eq a => [[a]] -> [[a]]
agrupa [] = []
agrupa xss
    | [] `elem` xss = []
    | otherwise     = primeros xss : agrupa (restos xss)
    where primeros = map head
          restos   = map tail

-- 6.8 - Números con dígitos pares
-- -----------------------------------------------------------------------------
-- 6.8.1 - Definir, por recursión, la función
--
-- superpar :: Int -> Bool
--
-- tal que (superar n) se verifica si n es un número par tal que todos sus 
-- dígitos son pares. Por ejemplo,
--
-- superpar 426 == True
-- superpar 456 == False

superpar :: Int -> Bool
superpar n | n < 10 = even n
           | otherwise = even n && superpar (n `div` 10)

-- 6.8.2 - Definir, por comprensión, la función
--
-- superpar2 :: Int -> Bool
--
-- tal que (superpar2 n) se verifica si n es un número par tal que todos sus 
-- dígitos son pares. Por ejemplo,
--
-- superpar2 426 == True
-- superpar2 456 == False

superpar2 :: Int -> Bool
superpar2 n = and [even d | d <- digitos n]

-- Donde (digitos n) es la lista de los dígitos de n.

digitos :: Int -> [Int]
digitos n = [read [d] | d <- show n]

-- 6.8.3 - Definir, por recursión sobre los dígitos, la función
--
-- superpar3 :: Int -> Bool
--
-- tal que (superpar3 n) se verifica si n es un número par tal que todos sus 
-- dígitos son pares. Por ejemplo,
--
-- superpar3 426 == True
-- superpar3 456 == False

superpar3 :: Int -> Bool
superpar3 n = sonPares (digitos n)
    where sonPares [] = True
          sonPares (d:ds) = even d && sonPares ds

-- 6.8.4 - Definir, usando all, la función
--
-- superpar4 :: Int -> Bool
--
-- tal que (superpar4 n) se verifica si n es un número par tal que todos sus 
-- dígitos son pares. Por ejemplo,
--
-- superpar4 426 == True
-- superpar4 456 == False

superpar4 :: Int -> Bool
superpar4 n = all even (digitos n)

-- 6.8.5 - Definir, usando filter, la función
--
-- superpar5 :: Int -> Bool
--
-- tal que (superpar5 n) se verifica si n es un número par tal que todos sus 
-- dígitos son pares. Por ejemplo,
-- 
-- superpar5 426 == True
-- superpar5 456 == False

superpar5 :: Int -> Bool
superpar5 n = filter even (digitos n) == digitos n

-- 6.9 - Lista de los valores de los elementos que cumplen una propiedad
-- -----------------------------------------------------------------------------
-- Se considera la función
--
-- filtraAplica :: (a -> b) -> (a -> Bool) -> [a] -> [b]
--
-- tal que (filtraAplica f p xs) es la lista obtenida aplicándole a los 
-- elementos de xs que cumplen el predicado p la función f. Por ejemplo,
--
-- filtraAplica (4+) (<3) [1..7] => [5,6]
--
-- Se pide, definir la función
--
-- 1. por comprensión,
-- 2. usando map y filter
-- 3. por recursión y
-- 4. por plegado (con foldr).

filtraAplica_1 :: (a -> b) -> (a -> Bool) -> [a] -> [b]
filtraAplica_1 f p xs = [f x | x <- xs, p x]

-- La definición con map y filter es

filtraAplica_2 :: (a -> b) -> (a -> Bool) -> [a] -> [b]
filtraAplica_2 f p xs = map f (filter p xs)

-- La definición por recursión es

filtraAplica_3 :: (a -> b) -> (a -> Bool) -> [a] -> [b]
filtraAplica_3 f p [] = []
filtraAplica_3 f p (x:xs) | p x = f x : filtraAplica_3 f p xs
                          | otherwise = filtraAplica_3 f p xs

-- La definición por plegado es

filtraAplica_4 :: (a -> b) -> (a -> Bool) -> [a] -> [b]
filtraAplica_4 f p = foldr g []
                     where g x y | p x = f x : y
                                 | otherwise = y

-- La definición por plegado usando lambda es

filtraAplica_4' :: (a -> b) -> (a -> Bool) -> [a] -> [b]
filtraAplica_4' f p =
    foldr (\x y -> if p x then (f x : y) else y) []

-- 6.10 - Máximo elemento de una lista
-- -----------------------------------------------------------------------------
-- 6.10.1 - Definir, mediante recursión, la función
--
-- maximumR :: Ord a => [a] -> a
--
-- tal que (maximunR xs) es el máximo de la lista xs. Por ejemplo,
--
-- maximumR [3,7,2,5] == 7
--
-- Nota: La función maximumR es equivalente a la predefinida maximun.

maximumR :: Ord a => [a] -> a
maximumR [x] = x
maximumR (x:y:ys) = max x (maximumR (y:ys))

-- 6.10.2 - La función de plegado foldr1 está definida por
--
-- foldr1 :: (a -> a -> a) -> [a] -> a
-- foldr1 _ [x] = x
-- foldr1 f (x:xs) = f x (foldr1 f xs)
--
-- Definir, mediante plegado con foldr1, la función
--
-- maximumP :: Ord a => [a] -> a
--
-- tal que (maximumR xs) es el máximo de la lista xs. Por ejemplo,
--
-- maximumP [3,7,2,5] == 7
--
-- Nota: La función maximumP es equivalente a la predefinida maximum.

maximumP :: Ord a => [a] -> a
maximumP = foldr1 max

-- 6.11 - Mínimo elemento de una lista
-- -----------------------------------------------------------------------------
-- Definir, mediante plegado con foldr1, la función
--
-- minimunP :: Ord a => [a] -> a
--
-- tal que (minimunP xs) es el máximo de la lista xs. Por ejemplo,
--
-- minimunP [3,7,2,5] == 2
--
-- Nota: La función minimunP es equivalente a la predefinida minimun.

minimunP :: Ord a => [a] -> a
minimunP = foldr1 min