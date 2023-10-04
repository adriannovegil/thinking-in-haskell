module OrdenSuperior where

import Test.QuickCheck
import Data.List (inits, tails)

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

-- 6.12 - Inversa de una lista
-- -----------------------------------------------------------------------------
-- 6.12.1 - Definir, mediante revursión, la función
--
-- inversaR :: [a] -> [a]
--
-- tal que (inversaR xs) es la inversa de la lista xs. Por ejemplo,
--
-- inversaR [3,5,2,4,7] == [7,4,2,5,3]

inversaR :: [a] -> [a]
inversaR [] = []
inversaR (x:xs) = (inversaR xs) ++ [x]

-- 6.12.2 - Definir, mediante plegado, la función
--
-- inversaP :: [a] -> [a]
--
-- tal que (inversaP xs) es la inversa de la lista xs. Por ejemplo,
--
-- inversaP [3,5,2,4,7] == [7,4,2,5,3]

inversaP :: [a] -> [a]
inversaP = foldr f []
    where f x y = y ++ [x]

-- La definición anterior puede simplificarse a

inversaP_2 :: [a] -> [a]
inversaP_2 = foldr f []
    where f x = (++ [x])

-- 6.12.3 - Definir, por recursión con acumulación, la función
--
-- inversaR' :: [a] -> [a]
--
-- tal que (inversaR' xs) es la inversa de la lista xs. Por ejemplo,
--
-- inversaR' [3,5,2,4,7] == [7,4,2,5,3]

inversaR' :: [a] -> [a]
inversaR' xs = inversaAux [] xs
    where inversaAux ys [] = ys
          inversaAux ys (x:xs) = inversaAux (x:ys) xs

-- 6.12.4 - La función de plegado foldl está definida por
--
-- foldl :: (a -> b -> a) -> a -> [b] -> a
-- foldl f ys xs = aux ys xs
--     where aux ys [] = ys
--           aux ys (x:xs) = aux (f ys x) xs
--
-- Definir, mediante plegado con foldl, la función
--
-- inversaP' :: [a] -> [a]
--
-- tal que (inversaP' xs) es la inversa de la lista xs. Por ejemplo,
--
-- inversaP' [3,5,2,4,7] == [7,4,2,5,3]

inversaP' :: [a] -> [a]
inversaP' = foldl f []
    where f ys x = x:ys

-- La definición anterior puede simplificarse lambda:

inversaP'_2 :: [a] -> [a]
inversaP'_2= foldl (\ys x -> x:ys) []

-- La definición anterior puede simplificarse usando flip

inversaP'_3 :: [a] -> [a]
inversaP'_3 = foldl (flip(:)) []

-- 6.12.5 - Comprobar con QuickCheck que las funciones reverse, inversaP e 
-- inversaP' son equivalentes

prop_inversa :: Eq a => [a] -> Bool
prop_inversa xs =
    inversaP xs == ys &&
    inversaP' xs == ys
    where ys = reverse xs

--La comprobación es
--
-- ghci> quickCheck prop_inversa
-- +++ OK, passed 100 tests.
--
-- 6.12.6 - Comparar la eficiencia de inversaP e inversaP' calculando el 
-- tiempo y el espacio usado en evaluar las siguientes expresiones:
--
-- head (inversaP [1..100000])
-- head (inversaP' [1..100000])
--
-- La sesión es
--
-- ghci> :set +s
-- ghci> head (inversaP [1..100000])
-- 100000
-- (0.07 secs, 21,804,464 bytes)
-- ghci> head (inversaP' [1..100000])
-- 100000
-- (0.02 secs, 11,261,696 bytes)
-- ghci>  :unset +s
--
-- 6.13 - Número correspondiente a la lista de sus cifras
-- -----------------------------------------------------------------------------
-- 6.13.1 - Definir, por recursión con acumulador, la función
--
-- dec2entR :: [Int] -> Int
--
-- tal que (dec2entR xs) es el entero correspondiente a la expresión decimal 
-- xs. Por ejemplo,
--
-- dec2entR [2,3,4,5] == 2345

dec2entR :: [Int] -> Int
dec2entR xs = dec2entR' 0 xs
    where dec2entR' a [] = a
          dec2entR' a (x:xs) = dec2entR' (10*a+x) xs

-- 6.13.2 - Definir, por plegado con foldl, la función
--
-- dec2entP :: [Int] -> Int
--
-- tal que (dec2entP xs) es el entero correspondiente a la expresión decimal 
-- xs. Por ejemplo
--
-- dec2entP [2,3,4,5] == 2345

dec2entP :: [Int] -> Int
dec2entP = foldl f 0
    where f a x = 10*a+x

-- La definición puede simplificarse usando lambda:

dec2entP' :: [Int] -> Int
dec2entP' = foldl (\a x -> 10*a+x) 0

-- 6.14 - Suma de valores de una aplicación a una lista
-- -----------------------------------------------------------------------------
-- 6.14.1 - Definir, por recursión, la función
--
-- sumaR :: Num b => (a -> b) -> [a] -> b
--
-- tal que (suma f xs) es la suma de los valores obtenido aplicando la función 
-- f a los elementos de la lista xs. Por ejemplo,
--
-- sumaR (*2) [3,5,10] == 36
-- sumaR (/10) [3,5,10] == 1.8

sumaR :: Num b => (a -> b) -> [a] -> b
sumaR f []     = 0
sumaR f (x:xs) = f x + sumaR f xs

-- 6.14.2 - Definir, por plegado, la función
--
-- sumaP :: Num b => (a -> b) -> [a] -> b
--
-- tal que (suma f xs) es la suma de los valores obtenido aplicando la función 
-- f a los elementos de la lista xs. Por ejemplo,
--
-- sumaP (*2) [3,5,10] == 36
-- sumaP (/10) [3,5,10] == 1.8

sumaP :: Num b => (a -> b) -> [a] -> b
sumaP f = foldr (\x y -> (f x) + y) 0

-- 6.15 - Redefinición de la función map usando foldr
-- -----------------------------------------------------------------------------
-- 6.15.1 - Redefinir, por recursión, la función map. Por ejemplo,
--
-- mapR (+2) [1,7,3] == [3,9,5]

mapR :: (a -> b) -> [a] -> [b]
mapR f []     = []
mapR f (x:xs) = f x : mapR f xs

-- 6.15.2 - Redefinir, usando foldr, la función map. Por ejemplo, 
--
-- mapP (+2) [1,7,3] == [3,9,5]

mapP :: (a -> b) -> [a] -> [b]
mapP f = foldr g []
         where g x xs = f x : xs

-- La definición por plegado usando lambda es

mapP' :: (a -> b) -> [a] -> [b]
mapP' f = foldr (\x y -> f x:y) []

-- Otra definición es

mapP'' :: (a -> b) -> [a] -> [b]
mapP'' f = foldr ((:) . f) []

-- 6.16 - Redefinición de la función filter usando foldr
-- -----------------------------------------------------------------------------
-- 6.16.1 - Redefinir, por recursión, la función filter. Por ejemplo,
--
-- filterR (<4) [1,7,3,2] => [1,3,2]

filterR :: (a -> Bool) -> [a] -> [a]
filterR p [] = []
filterR p (x:xs) | p x       = x : filterR p xs
                 | otherwise = filterR p xs

-- 6.16.2 - Redefinir, usando foldr, la función filter. Por ejemplo,
--
-- filterP (<4) [1,7,3,2] => [1,3,2]

filterP :: (a -> Bool) -> [a] -> [a]
filterP p = foldr g []
    where g x y | p x       = x:y
                | otherwise = y

-- La definición por plegado y lambda es

filterP' :: (a -> Bool) -> [a] -> [a]
filterP' p = foldr (\x y -> if (p x) then (x:y) else y) []

-- 6.17 - Suma de las sumas de las listas de una lista de listas
-- -----------------------------------------------------------------------------
-- 6.17.1 - Definir, mediante recursión, la función
--
-- sumllR :: Num a => [[a]] -> a
--
-- tal que (sumllR xss) es la suma de las sumas de las listas de xss. Por 
-- ejemplo,
--
-- sumllR [[1,3],[2,5]] == 11

sumllR :: Num a => [[a]] -> a
sumllR []       = 0
sumllR (xs:xss) = sum xs + sumllR xss

-- 6.17.2 - Definir, mediante plegado, la función
--
-- sumllP :: Num a => [[a]] -> a
--
-- tal que (sumllP xss) es la suma de las sumas de las listas de xss. Por 
-- ejemplo,
--
-- sumllP [[1,3],[2,5]] == 11

sumllP :: Num a => [[a]] -> a
sumllP = foldr f 0
    where f xs n = sum xs + n

-- La definición anterior puede simplificarse usando lambda

sumllP' :: Num a => [[a]] -> a
sumllP' = foldr (\xs n -> sum xs + n) 0

-- 6.17.3 - Definir, mediante recursión con acumulador, la función
--
-- sumllA :: Num a => [[a]] -> a
--
-- tal que (sumllA xss) es la suma de las sumas de las listas de xss. Por 
-- ejemplo,
--
-- sumllA [[1,3],[2,5]] == 11

sumllA :: Num a => [[a]] -> a
sumllA xs = aux 0 xs
    where aux a []       = a
          aux a (xs:xss) = aux (a + sum xs) xss

-- 6.17.4 - Definir, mediante plegado con foldl, la función
--
-- sumllAP :: Num a => [[a]] -> a
--
-- tal que (sumllAP xss) es la suma de las listas de xss. Por ejemplo,
--
-- sumllAP [[1,3],[2,5]] == 11

sumllAP :: Num a => [[a]] -> a
sumllAP = foldl (\a xs -> a + sum xs) 0

-- 6.18 - Lista obtenida borrando las ocurrencias de un elemento
-- -----------------------------------------------------------------------------
-- 6.18.1 - Definir, mediante recursión, la función
--
-- borraR :: Eq a => a -> a -> [a]
--
-- tal que (borraR xs) es la lista obtenida borrand las ocurrencias de y en xs. 
-- Por ejemplo,
--
-- borraR 5 [2,3,5,6]   == [2,3,6]
-- borraR 5 [2,3,5,6,5] == [2,3,6]
-- borraR 7 [2,3,5,6,5] == [2,3,5,6,5]

borraR :: Eq a => a -> [a] -> [a]
borraR z [] = []
borraR z (x:xs) | z == x    = borraR z xs
                | otherwise = x : borraR z xs

-- 6.18.2 - Definir, mediante plegado, la función
--
-- borraP :: Eq a => a -> a -> [a]
--
-- tal que (borraP y xs) es la lista obtenida borrando las ocurrencias de y en 
-- xs. Por ejemplo,
--
-- borraP 5 [2,3,5,6]   == [2,3,6]
-- borraP 5 [2,3,5,6,5] == [2,3,6]
-- borraP 7 [2,3,5,6,5] == [2,3,5,6,5]

borraP :: Eq a => a -> [a] -> [a]
borraP z = foldr f []
    where f x y | z == x    = y
                | otherwise = x:y

-- La definición por plegado com Lambda es

borraP' :: Eq a => a -> [a] -> [a]
borraP' z = foldr (\x y -> if z==x then y else x:y) []

-- 6.19 - Diferencia de dos listas
-- -----------------------------------------------------------------------------
-- 6.19.1 - Definir, mediante recursión, la función
--
-- diferenciaR :: Eq a => [a] -> [a] -> [a]
--
-- tal que (difrenciaR xs ys) es la diferencia de conjunto xs e ys; es decir el 
-- conjunto de los elementos de xs que no pertenecen a ys. Por ejemplo,
--
-- diferenciaR [2,3,5,6] [5,2,7] == [3,6]

diferenciaR :: Eq a => [a] -> [a] -> [a]
diferenciaR xs ys = aux xs xs ys
    where aux a xs []     = a
          aux a xs (y:ys) = aux (borraR y a) xs ys

-- La definición, para aproximarse al patrón de plegado, se puede escribir como

diferenciaR' :: Eq a => [a] -> [a] -> [a]
diferenciaR' xs ys = aux xs xs ys
    where aux a xs []     = a
          aux a xs (y:ys) = aux (flip borraR a y) xs ys

-- 6.19.2 - Definir, mediante plegado con foldl, la función
--
-- diferenciaP :: Eq a => [a] -> [a] -> [a]
--
-- tal que (diferencia xs ys) es la diferencia del conjunto xs e ys; es decir 
-- el conjunto de los elementos de xs que no pertenecen a ys. Por ejemplo,
--
-- diferenciaP [2,3,5,6] [5,2,7] == [3,6]

diferenciaP :: Eq a => [a] -> [a] -> [a]
diferenciaP xs ys = foldl (flip borraR) xs ys

-- La definición anterior puede simplificarse a

diferenciaP' :: Eq a => [a] -> [a] -> [a]
diferenciaP' = foldl (flip borraR)

-- 6.20 - Producto de los números que verifican una propiedad
-- -----------------------------------------------------------------------------
-- 6.20.1 - Definir mediante plegado la función
--
-- producto :: Num a => [a] -> a
--
-- tal que (producto xs) es el producto de los elementos de la lista xs. Por 
-- ejemplo,
--
-- producto [2,1,-3,4,5,-6] == 720

producto :: Num a => [a] -> a
producto = foldr (*) 1

-- 6.20.2 - Definir mediante plegado la función
--
-- productoPred :: Num a => (a -> Bool) -> [a] -> a
--
-- tal que (productoPred p xs) es el producto de los elementos de la lista xs 
-- que verifican el predicado p. Por ejemplo,
--
-- productoPred even [2,1,-3,4,-5,6] == 48

productoPred :: Num a => (a -> Bool) -> [a] -> a
productoPred p = foldr (\x y -> if p x then x*y else y) 1

-- 6.20.3 - Definir la función
--
-- productoPos :: (Num a, Ord a) => [a] -> a
--
-- tal que (productoPos xs) es el producto de los elementos estríctamente 
-- positivos de la lista xs. Por ejemplo,
--
-- productoPos [2,1,-3,4,-5,6] == 48

productoPos :: (Num a, Ord a) => [a] -> a
productoPos = productoPred (>0)

-- 6.21 - Las cabezas y las colas de una lista
-- -----------------------------------------------------------------------------
-- 6-21-1 - Se denomina cola de una lista xs a una sublista no vacía de xs 
-- formada por un elemento y los siguientes hasta el final. Por ejemplo, [3,4,5] 
-- es una cola de la lista [1,2,3,4,5].
-- Definir la función
--
-- colas :: [a] -> [[a]]
--
-- tal que (colas xs) es la lista de las colas de la lista xs. Por ejemplo,
--
-- colas [] == [[]]
-- colas [1,2] == [[1,2],[2],[]]
-- colas [4,1,2,5] == [[4,1,2,5],[1,2,5],[2,5],[5],[]]

colas :: [a] -> [[a]]
colas []     = [[]]
colas (x:xs) = (x:xs) : colas xs

-- 6.21.2 - Comprobar con QuickCheck que las funciones colas y tails son 
-- equivalentes.

prop_colas :: [Int] -> Bool
prop_colas xs = colas xs == tails xs

-- La comprobación es
--
-- ghci> quickCheck prop_colas
-- +++ OK, passed 100 tests.
--
-- 6.21.3 - Se denomina cabeza de una lsta xs a una sublista no vacía de xs formada por
-- el primer elemento y los siguientes hasta uno dado. Por ejemplo, [1,2,3] es 
-- una cabeza de [1,2,3,4,5].
-- Definir, por recursión, la función
--
-- cabezas :: [a] -> [[a]]
--
-- tal que (cabeza xs) es la lista de las cabezas de la lista xs. Por ejemplo,
--
-- cabezas []          == [[]]
-- cabezas [1,4]       == [[],[1],[1,4]]
-- cabezas [1,4,5,2,3] == [[],[1],[1,4],[1,4,5],[1,4,5,2],[1,4,5,2,3]]

cabezas :: [a] -> [[a]]
cabezas []     = [[]]
cabezas (x:xs) = [] : [x:ys | ys <- cabezas xs]

-- 6.21.4 - Definir, por plegado, la función
--
-- cabezasP :: [a] -> [[a]]
--
-- tal que (cabezasP xs) es la lista de las cabezasP de la lista xs. Por 
-- ejemplo,
--
-- cabezasP []          == [[]]
-- cabezasP [1,4]       == [[],[1],[1,4]]
-- cabezasP [1,4,5,2,3] == [[],[1],[1,4],[1,4,5],[1,4,5,2],[1,4,5,2,3]]

cabezasP :: [a] -> [[a]]
cabezasP = foldr (\x y -> [x]:[x:ys | ys <- y]) []

-- 6.21.5 - Definir, mediante funciones de orden superior, la función
--
-- cabezasS :: [a] -> [[a]]
--
-- tal que (cabezasS xs) es la lista de las cabezasS de la lista xs. Por 
-- ejemplo,
--
-- cabezasS []          == [[]]
-- cabezasS [1,4]       == [[],[1],[1,4]]
-- cabezasS [1,4,5,2,3] == [[],[1],[1,4],[1,4,5],[1,4,5,2],[1,4,5,2,3]]

cabezasS :: [a] -> [[a]]
cabezasS xs = reverse (map reverse (colas (reverse xs)))

-- La anterior definición puede escribirse sin argumentos como

cabezasS' :: [a] -> [[a]]
cabezasS' = reverse . map reverse . (colas . reverse)

-- 6.21.6 - Comprobar con QuickCheck que las funciones cabezas y inits son 
-- equivalentes.

prop_cabezas :: [Int] -> Bool
prop_cabezas xs = cabezas xs == inits xs

-- La comprobación es

-- ghci> quickCheck prop_cabezas
-- +++ OK, passed 100 tests.