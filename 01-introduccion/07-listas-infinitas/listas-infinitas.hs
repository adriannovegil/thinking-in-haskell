module ListasInfinitas where

import Test.QuickCheck

main :: IO ()
main = return ()

-- 7.1 - Lista obtenida repitiendo un elementos
-- -----------------------------------------------------------------------------
--
-- 7.1.1 - Definir, por recursión, la función
--
-- repite :: a -> [a]
--
-- tal que (repite x) es la lista infnita cuyos elementos son x. Por ejemplo,
--
-- repite 5 == [5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,...
-- take 3 (repite 5) == [5,5,5]
--
-- Nota: La función repite es equivalente a la función repeat definida en el 
-- preludio de Haskell.

repite :: a -> [a]
repite x = x : repite x

-- 7.1.2 - Definir, por comprensión, la función
--
-- repiteC :: a -> [a]
--
-- tal que (repiteC x) es la lista infinita cuyos elementos son x. Por ejemplo,
--
-- repiteC 5 == [5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,...
-- take 3 (repiteC 5) == [5,5,5]

repiteC :: a -> [a]
repiteC x = [x | _ <- [1..]]

-- 7.1.3 - Definir, por recursión, la función
--
-- repiteFinita :: Int-> a -> [a]
--
-- tal que (repiteFinita n x) es la lista con n elementos iguales a x. Por 
-- ejemplo,
--
-- repiteFinita 3 5 == [5,5,5]
--
-- Nota: La función repiteFinita es equivalente a la función replicate definida
-- en el preludio de Haskell.

repiteFinita :: Int -> a -> [a]
repiteFinita 0 x = []
repiteFinita n x = x : repiteFinita (n-1) x

-- 7.1.4 - Definir, por comprensión, la función
--
-- repiteFinitaC :: Int-> a -> [a]
--
-- tal que (repiteFinitaC n x) es la lista con n elementos iguales a x. Por 
-- ejemplo,
--
-- repiteFinitaC 3 5 == [5,5,5]

repiteFinitaC :: Int -> a -> [a]
repiteFinitaC n x = [x | _ <- [1..n]]

-- 7.1.5 - Definir, usando repite, la función
--
-- repiteFinita' :: Int-> a -> [a]
--
-- tal que (repiteFinita' n x) es la lista con n elementos iguales a x. Por 
-- ejemplo,
--
-- repiteFinita' 3 5 == [5,5,5]

repiteFinita' :: Int -> a -> [a]
repiteFinita' n x = take n (repite x)

-- 7.2 - Lista obtenida repitiendo cada elemento según su posición
-- -----------------------------------------------------------------------------
-- 7.2.1 - Definir, por comprensión, la función
--
-- ecoC :: [a] -> [a]
--
-- tal que (ecoC xs) es la lista obtenida a partir de la lista xs repitiendo 
-- cada elemento tantas veces como indica su posición: el primer elemento se 
-- repite 1 vez, el segundo 2 veces y así sucesivamente. Por ejemplo,
--
-- ecoC "abcd"          == "abbcccdddd"
-- take 10 (ecoC [1..]) == [1,2,2,3,3,3,4,4,4,4]

ecoC :: [a] -> [a]
ecoC xs = concat [replicate i x | (i,x) <- zip [1..] xs]

-- 7.2.2 - Definir, por recursión, la función
--
-- ecoR :: [a] -> [a]
--
-- tal que (ecoR xs) es la lista obtenida a partir de la lista xs repitiendo 
-- cada elemento tantas veces como indica su posición: el primer elemento se 
-- repite 1 vez, el segundo 2 veces y así sucesivamente. Por ejemplo,
--
-- ecoR "abcd" == "abbcccdddd"
-- take 10 (ecoR [1..]) == [1,2,2,3,3,3,4,4,4,4]

ecoR :: [a] -> [a]
ecoR xs = aux 1 xs
    where aux n []     = []
          aux n (x:xs) = replicate n x ++ aux (n+1) xs

-- 7.3 - Potencias de un número menores que otro lado
-- -----------------------------------------------------------------------------
-- Definir, usando takeWhile y map, la función
--
-- potenciasMenores :: Int -> Int -> [Int]
--
-- tal que (potenciasMenores x y) es la lista de las potencias de x menores que 
-- y. Por ejemplo,
--
-- potenciasMenores 2 1000 == [2,4,8,16,32,64,128,256,512]

potenciasMenores :: Int -> Int -> [Int]
potenciasMenores x y = takeWhile (<y) (map (x^) [1..])

-- 7.4 - Mútiplos cuyos dígitos verifican una propiedad
-- -----------------------------------------------------------------------------
-- 
-- Problema 303 del proyecto Euler. Definir la función
--
-- multiplosRestringidos :: Int -> (Int -> Bool) -> [Int]
-- 
-- tal que (multiplosRestringidos n x) es la lista de los múltiplos de n tales 
-- que todos sus dígitos verifican la propiedad p. Por ejemplo,
--
-- take 4 (multiplosRestringidos 5 (<=3)) == [10,20,30,100]
-- take 5 (multiplosRestringidos 3 (<=4)) == [3,12,21,24,30]
-- take 5 (multiplosRestringidos 3 even) == [6,24,42,48,60]

multiplosRestringidos :: Int -> (Int -> Bool) -> [Int]
multiplosRestringidos n p =
    [y | y <- [n,2*n..], all p (digitos y)]

-- donde (digitos n) es la lista de los dígitos de n. Por ejemplo,
--
-- digitos 327 == [3,2,7]

digitos :: Int -> [Int]
digitos n = [read [x] | x <- show n]

-- 7.5 - Aplicación iterada de una función a un elemento
-- -----------------------------------------------------------------------------
--
-- itera :: (a -> a) -> a -> [a]
--
-- tal que (itera f x) es la lista cuyo primer elemento es x y los siguientes 
-- elementos se calculan aplicando la función f al elemento anterior. Por ejemplo,
--
-- ghci> itera (+1) 3
-- [3,4,5,6,7,8,9,10,11,12,Interrupted!
-- ghci> itera (*2) 1
-- [1,2,4,8,16,32,64,Interrupted!
-- ghci> itera (`div` 10) 1972
-- [1972,197,19,1,0,0,0,0,0,0,Interrupted!
--
-- Nota: La función itera es equivalente a la función iterate definida en el 
-- preludio de Haskell 

itera :: (a -> a) -> a -> [a]
itera f x = x : itera f (f x)

-- 7.6 - Agrupamiento de elementos consecutivos
-- -----------------------------------------------------------------------------
-- 7.6.1 - Definir, por recursión, la función
--
-- agrupa :: Int -> [a] -> [[a]]
--
-- tal que (agrupa n xs) es la lista formada por listas de n elementos 
-- consecutivos de la lista xs (salvo posiblemente la última que puede tener 
-- menos de n elementos). Por ejemplo,
--
-- ghci> agrupa 2 [3,1,5,8,2,7]
-- [[3,1],[5,8],[2,7]]
-- ghci> agrupa 2 [3,1,5,8,2,7,9]
-- [[3,1],[5,8],[2,7],[9]]
-- ghci> agrupa 5 "todo necio confunde valor y precio"
-- ["todo ","necio"," conf","unde ","valor"," y pr","ecio"]

agrupa :: Int -> [a] -> [[a]]
agrupa n [] = []
agrupa n xs = take n xs : agrupa n (drop n xs)

-- 7.6.2 - Definir, la manera no recursiva, la función
--
-- agrupa' :: Int -> [a] -> [[a]]
--
-- tal que (agrupa' n xs) es la lista formada por listas de n elementos 
-- consecutivos de la lista xs (salvo posiblemente la última que puede tener
-- menor de n elementos). Por ejemplo,
--
-- ghci> agrupa' 2 [3,1,5,8,2,7]
-- [[3,1],[5,8],[2,7]]
-- ghci> agrupa' 2 [3,1,5,8,2,7,9]
-- [[3,1],[5,8],[2,7],[9]]
-- ghci> agrupa' 5 "todo necio confunde valor y precio"
-- ["todo ","necio"," conf","unde ","valor"," y pr","ecio"]

agrupa' :: Int -> [a] -> [[a]]
agrupa' n = takeWhile (not . null)
        . map (take n)
        . iterate (drop n)

-- Puede verse su funcionamiento en el siguiente ejemplo,
--
-- iterate (drop 2) [5..10]
-- ==> [[5,6,7,8,9,10],[7,8,9,10],[9,10],[],[],...
-- map (take 2) (iterate (drop 2) [5..10])
-- ==> [[5,6],[7,8],[9,10],[],[],[],[],...
-- takeWhile (not . null) (map (take 2) (iterate (drop 2) [5..10]))
-- ==> [[5,6],[7,8],[9,10]]
--
-- 7.6.3 - Definir, y comprobar, con QuickCheck las dos propiedades que 
-- caracterizan a la función agrupa:
--
-- * todos los grupos tienen que tener la longitud determinada (salvo el último
--   que puede tener una longitud menor) y
-- * combinando todos los grupos se obtien la lista inicial.

prop_AgrupaLongitud :: Int -> [Int] -> Property
prop_AgrupaLongitud n xs =
    n > 0 && not (null gs) ==>
      and [length g == n | g <- init gs] &&
      0 < length (last gs) && length (last gs) <= n
    where gs = agrupa n xs

-- La comprobación es
--
-- ghci> quickCheck prop_AgrupaLongitud
-- OK, passed 100 tests.
--
-- La segunda propiedad es

prop_AgrupaCombina :: Int -> [Int] -> Property
prop_AgrupaCombina n xs =
    n > 0 ==> concat (agrupa n xs) == xs

-- La comprobación es
-- 
-- ghci> quickCheck prop_AgrupaCombina
-- OK, passed 100 tests.
--
-- 7.7 - La sucesión de Collatz
-- -----------------------------------------------------------------------------
-- Se considera la siguiente operación, aplicable a cualquier número entero 
-- positivo:
--
-- * Si el número es par, se divide entre 2.
-- * Si el número es impar, se multiplica por 3 y se suma 1.
--
-- Dado un número cualquiera, podemos considerar su órbita; es decir, las 
-- imágenes sucesivas al iterar la función. Por ejemplo, la órbita de 13 es 13, 
-- 40, 20, 10, 5, 16, 8, 4, 2, 1, 4, 2, 1, ... Si observamos este ejemplo, la 
-- órbita de 13 es periódica; es decir, se repite indefinidamente a partir de 
-- un momento dado. La conjetura de Collatz dice que siempre alcanzaremos el 1 
-- para cualquier número con el que comencemos. Ejemplos:
--
-- * Empezando en n = 6 se obtiene 6, 3, 10, 5, 16, 8, 4, 2, 1.
-- * Empezando en n = 11 se obtiene: 11, 34, 17, 52, 26, 13, 40, 20, 10, 5, 16, 
--   8, 4, 2, 1.
-- * Empezando en n = 27, la sucesión tiene 112 pasos, llegando hasta 9232 
--   antes de descender a 1: 27, 82, 41, 124, 62, 31, 94, 47, 142, 71, 214, 
--   107, 322, 161, 484, 242, 121, 364, 182, 91, 274, 137, 412, 206, 103, 310,
--   155, 466, 233, 700, 350, 175, 526, 263, 790, 395, 1186, 593, 1780, 890, 
--   445, 1336, 668, 334, 167, 502, 251, 754, 377, 1132, 566, 283, 850, 425, 
--   1276, 638, 319, 958, 479, 1438, 719, 2158, 1079, 3238, 1619, 4858, 2429, 
--   7288, 3644, 1822, 911, 2734, 1367, 4102, 2051, 6154, 3077, 9232, 4616, 
--   2308, 1154, 577, 1732, 866, 433, 1300, 650, 325, 976, 488, 244, 122, 61, 
--   184, 92, 46, 23, 70, 35, 106, 53, 160, 80, 40, 20, 10, 5, 16, 8, 4, 2, 1.
--
-- 7.7.1 - Definir la función
--
-- siguiente :: Integer -> Integer
--
-- tal que (siguiente n) es el siguiente de n en la sucesión de Collatz. Por 
-- ejemplo,
--
-- siguiente 13 == 40
-- siguiente 40 == 20

siguiente n | even n    = n `div` 2
            | otherwise = 3*n+1

-- 7.7.2 - Definir, por recursión, la función
--
-- collatz :: Integer -> [Integer]
--
-- tal que (collatz n) es la órbita de Collatz de n hasta alcanzar el 1. Por 
-- ejemplo,
--
-- collatz 13 == [13,40,20,10,5,16,8,4,2,1]

collatz :: Integer -> [Integer]
collatz 1 = [1]
collatz n = n : collatz (siguiente n)

-- 7.7.3 - Definir, sin recursión, la función
--
-- collatz' :: Integer -> [Integer]
--
-- tal que (collatz' n)

collatz' :: Integer -> [Integer]
collatz' n = (takeWhile (/=1) (iterate siguiente n)) ++ [1]

-- 7.7.4 - Definir la función
--
-- menorCollatzMayor :: Int -> Integer
--
-- tal que (menorCollatzMayor x) es el menor número cuya órbita de Collatz 
-- tiene más de x elementos. Por ejemplo,
--
-- menorCollatzMayor 100 == 27

menorCollatzMayor :: Int -> Integer
menorCollatzMayor x = head [y | y <- [1..], length (collatz y) > x]

-- 7.7.5 - Definir la función
--
-- menorCollatzSupera :: Integer -> Integer
--
-- tal que (menorCollatzSupera x) es el número cuya órbita de Collatz tiene 
-- algún elemento mayor que x. Por ejemplo,
--
-- menorCollatzSupera 100 == 15

menorCollatzSupera :: Integer -> Integer
menorCollatzSupera x =
    head [y | y <- [1..], maximum (collatz y) > x]

-- Otra definición alternativa es

menorCollatzSupera' :: Integer -> Integer
menorCollatzSupera' x = head [n | n <- [1..], t <- collatz' n, t > x]

-- 7.8 - Números primos
-- -----------------------------------------------------------------------------
-- 7.8.1 - Dfinir la constante
--
-- primos :: Integral a => [a]
--
-- tal que primos es la lista de los primos mediante la criba de Eratóstenes. 
-- Ejemplo,
--
-- take 10 primos == [2,3,5,7,11,13,17,19,23,29]

primos :: Integral a => [a]
primos = criba [2..]
    where criba [] = []
          criba (n:ns) = n : criba (elimina n ns)
          elimina n xs = [x | x <- xs, x `mod` n /= 0]

-- 7.9 - Descomposicióncomo suma de dos primos
-- -----------------------------------------------------------------------------
-- Definir la función
--
-- sumaDeDosPrimos :: Int -> [(Int,Int)]
--
-- tal que (sumaDeDosPrimos n) es la lista de las distintas descomposiciones de 
-- n como suma de dos números primos. Por ejemplo,

sumaDeDosPrimos :: Int -> [(Int,Int)]
sumaDeDosPrimos n =
    [(x,n-x) | x <- primosN, x < n-x, elem (n-x) primosN]
    where primosN = takeWhile (<=n) primos

-- donde primos está definida previamente.
-- El cálculo es
--
-- ghci> head [x | x <- [1..], length (sumaDeDosPrimos x) == 10]
-- 114
--
-- 7.10 - Números expresables como producto de dos primos
-- -----------------------------------------------------------------------------
-- Definir la función
-- 
-- esProductoDeDosPrimos :: Int -> Bool
--
-- tal que (esProductoDeDosPrimos n) se verifica si n es el producto de dos 
-- primos distintos. Por ejemplo,
--
-- esProductoDeDosPrimos 6 == True
-- esProductoDeDosPrimos 9 == False

esProductoDeDosPrimos :: Int -> Bool
esProductoDeDosPrimos n =
    [x | x <- primosN,
         mod n x == 0,
         div n x /= x,
         elem (div n x) primosN] /= []
    where primosN = takeWhile (<=n) primos

-- 7.11 - Números muy compuestos
-- -----------------------------------------------------------------------------
-- 7.11.1 - Un número es muy compuesto si tiene más divisores que sus 
-- anteriores. Por ejemplo, 12 es muy compuesto porque tiene 6 divisores 
-- (1,2,3,4,6,12) y todos los número del 1 al 11 tienen menos de 6 divisores.
-- Definir la función
--
-- esMuyCompuesto :: Int -> Bool
--
-- tal que (esMuyCompuesto x) se verifica si x es un número muy compuesto. Por
-- ejemplo,
--
-- esMuyCompuesto 24 == True
-- esMuyCompuesto 25 == False

esMuyCompuesto :: Int -> Bool
esMuyCompuesto x =
    and [numeroDivisores y < n | y <- [1..x-1]]
    where n = numeroDivisores x

-- donde se usan las siguientes funciones auxiliares:
--
-- * (numeroDivisores x) es el número de divisores de x. Por ejemplo,
--
-- numeroDivisores 24 == 8

numeroDivisores :: Int -> Int
numeroDivisores = length . divisores

-- * (divisores x) es la lista de los divisores de x. Por ejemplo,
--
-- divisores 24 == [1,2,3,4,6,8,12,24]

divisores :: Int -> [Int]
divisores x = [y | y <- [1..x], mod x y == 0]

-- Los primeros números muy compuestos son
--
-- ghci> take 14 [x | x <- [1..], esMuyCompuesto x]
-- [1,2,4,6,12,24,36,48,60,120,180,240,360,720]
--
-- 7.11.2 - Calcular el menor número muy compuesto de 4 cifras.
-- Solución: El cálculo del menor número muy compuesto de 4 cifras es
--
-- ghci> head [x | x <- [1000..], esMuyCompuesto x]
-- 1260
--
-- 7.11.3 - Definir la función
--
-- muyCompuesto :: Int -> Int
--
-- tal que (muyCompuesto n) es el n-ésimo número muy compuesto. Por ejemplo,
--
-- muyCompuesto 10 == 180

muyCompuesto :: Int -> Int
muyCompuesto n =
    [x | x <- [1..], esMuyCompuesto x] !! n 

-- 7.12 - Suma de números primos truncables
-- -----------------------------------------------------------------------------
-- Los siguientes ejercicios están basados en el problema 37 del proyecto Euler.
-- Un número primo es truncable si los números que se obtienen eliminado cifras, 
-- de derecha a izquierda, son primos. Por ejemplo, 599 es un primo truncable 
-- porque 599. 59 y 5 son primos; en cambio, 577 es un primo no truncable porque
-- 57 no es primo.
-- 7.12.1 - Definir la función
--
-- primoTruncable :: Int -> Bool
--
-- tal que (primoTruncable x) se verifica si x es un primo truncable. Por 
-- ejemplo,
--
-- primoTruncable 599 == True
-- primoTruncable 577 == False

primoTruncable :: Int -> Bool
primoTruncable x
    | x < 10 = primo x
    | otherwise = primo x && primoTruncable (x `div` 10)

-- donde se usan la función primo definida previamente.
-- 7.12.2 - Definir la función
--
-- sumaPrimosTruncables :: Int -> Int
--
-- tal que (sumaPrimosTruncables n) es la suma de los n primeros primos 
-- truncables. Por ejemplo,
--
-- sumaPrimosTruncables 10 == 249

sumaPrimosTruncables :: Int -> Int
sumaPrimosTruncables n =
    sum (take n [x | x <- primos, primoTruncable x])

-- 7.12.3 - Calcular la suma de los 20 primeros truncables.
-- Solución: El cálculo es
--
-- ghci> sumaPrimosTruncables 20
-- 2551
--
-- 7.13 - Primos permitables
-- -----------------------------------------------------------------------------
-- Un primo permutable es un número primo tal que todos los números obtenidos
-- permutandos sus cifras son primos. Por ejemplo, 337 es un primo permutable ya
-- que 337, 373 y 733 son primos.
-- Definir la función
--
-- primoPermutable :: Int -> Bool
--
-- tal que (primoPermutable x) se verifica si x es un primo permutable. Por 
-- ejemplo,
--
-- primoPermutable 17 == True
-- primoPermutable 19 == False

primoPermutable :: Int -> Bool
primoPermutable x = and [primo y | y <- permutacionesN x]

-- donde (primo x) es

primo :: Integral a => a -> Bool
primo x = x == head (dropWhile (<x) primos)

-- donde (permutacionesN x) es la lista de los números obtenidos permutando los
-- dígitos de x. Por ejemplo,
--
-- permutacionesN 325 == [325,235,253,352,532,523]

permutacionesN :: Int -> [Int]
permutacionesN x = [read ys | ys <- permutaciones (show x)]

-- Donde (permutaciones xs) es

permutaciones :: [a] -> [[a]]
permutaciones [] = [[]]
permutaciones (x:xs) =
    concat [intercala x ys | ys <- permutaciones xs]

-- e (intercala x ys) es 

intercala :: a -> [a] -> [[a]]
intercala x [] = [[x]]
intercala x (y:ys) = (x:y:ys) : [y:zs | zs <- intercala x ys]

-- 7.14 - Ordenación de los números enteros
-- -----------------------------------------------------------------------------
-- Los números enteros se pueden ordenar como sigue: 0, -1, 1, -2, 2, -3, 3, -4,
-- 4, -5, 5, -6, 6, -7, 7, ...
-- 7.14.1 - Definir, por comprensión, la constante
--
-- enteros :: [Int]
--
-- tal que enteros es la lista de los enteros con la ordenación anterior. Por 
-- ejemplo,
--
-- take 10 enteros == [0,-1,1,-2,2,-3,3,-4,4,-5]

enteros :: [Int]
enteros = 0 : concat [[-x,x] | x <- [1..]]

-- 7.14.2 - Definir, por iteración, la constante
--
-- enteros' :: [Int]
--
-- tal que enteros' es la lista de los enteros con la ordenación anterior. Por 
-- ejemplo,
--
-- take 10 enteros' == [0,-1,1,-2,2,-3,3,-4,4,-5]

enteros' :: [Int]
enteros' = iterate siguiente 0
    where siguiente x | x >= 0 = -x-1
                      | otherwise = -x

-- 7.14.3 - Definir, por selección con takeWhile, la función
--
-- posicion :: Int -> Int
--
-- tal que (posicion x) es la posición del entero x en la ordenación anterior.
-- Por ejemplo,
--
-- posicion 2 == 4

posicion :: Int -> Int
posicion x = length (takeWhile (/=x) enteros)

-- 7.14.4 - Definir, por recursión, la función
--
-- posicion1 :: Int -> Int
--
-- tal que (posicion1 x) es la posición del entero x en la ordenación anterior.
-- Por ejemplo,
--
-- posicion1 2 == 4

posicion1 :: Int -> Int
posicion1 x = aux enteros 0
 where aux (y:ys) n | x == y = n
                    | otherwise = aux ys (n+1)

-- 7.14.5 - Definir, por comprensión, la función
--
-- posicion2 :: Int -> Int
--
-- tal que (posicion2 x) es la posición del entero x en la ordenación anterior.
-- Por ejempllo,
--
-- posicion2 2 == 4

posicion2 :: Int -> Int
posicion2 x = head [n | (n,y) <- zip [0..] enteros, y == x]

-- 7.14.6 - Definir, sin búsqueda, la función
--
-- posicion3 :: Int -> Int
--
-- tal que (posicion3 x) es la posición del entero x en la ordenación anterior.
-- Por ejemplo,
--
-- posicion3 2 == 4

posicion3 :: Int -> Int
posicion3 x | x >= 0 = 2*x
            | otherwise = 2*(-x)-1

-- 7.15 - La sucesión de Hamming
-- -----------------------------------------------------------------------------
-- Los números de Hamming forman una sucesión estrictamente creciente de números
-- que cumplen las siguientes condiciones:
--
-- 1. El número 1 está en la sucesión.
-- 2. Si x está en la sucesión, entonces 2x, 3x y 5x también están.
-- 3. Ningún otro número está en la sucesión.
--
-- 7.15.1 - Definir la constante
--
-- hamming :: [Int]
--
-- tal que hamming es la sucesión de Hamming. Por ejemplo,
--
-- take 12 hamming == [1,2,3,4,5,6,8,9,10,12,15,16]

hamming :: [Int]
hamming = 1 : mezcla3 [2*i | i <- hamming]
                      [3*i | i <- hamming]
                      [5*i | i <- hamming]

-- donde se usan las siguientes funciones auxiliares
--
-- * (mezcla3 xs ys zs) es la lista obtenida mezclando las listas ordenadas xs, 
-- ys y zs y eliminando los elementos duplicados. Por ejemplo,
--
-- ghci> mezcla3 [2,4,6,8,10] [3,6,9,12] [5,10]
-- [2,3,4,5,6,8,9,10,12]

mezcla3 :: [Int] -> [Int] -> [Int] -> [Int]
mezcla3 xs ys zs = mezcla2 xs (mezcla2 ys zs)

-- * (mezcla2 xs ys zs) es la lista obtenida mezclando las listas ordenadas xs 
-- e ys y eliminando los elementos duplicados. Por ejemplo,
--
-- ghci> mezcla2 [2,4,6,8,10,12] [3,6,9,12]
-- [2,3,4,6,8,9,10,12]

mezcla2 :: [Int] -> [Int] -> [Int]
mezcla2 p@(x:xs) q@(y:ys) | x < y     = x:mezcla2 xs q
                          | x > y     = y:mezcla2 p ys
                          | otherwise = x:mezcla2 xs ys
mezcla2 []       ys                   = ys
mezcla2 xs       []                   = xs

-- 7.15.2 - Definir la función
--
-- divisoresEn :: Int -> [Int] -> Bool
--
-- tal que (divisoresEn x ys) se verifica si x se puede expresarse como un 
-- producto de potencias de elementos de ys. Por ejemplo,
--
-- divisoresEn 12 [2,3,5] == True
-- divisoresEn 14 [2,3,5] == False

divisoresEn :: Int -> [Int] -> Bool
divisoresEn 1 _                     = True
divisoresEn x []                    = False
divisoresEn x (y:ys) | mod x y == 0 = divisoresEn (div x y) (y:ys)
                     | otherwise    = divisoresEn x ys

-- 7.15.3 - Definir, usando divisoresEn, la constante
--
-- hamming' :: [Int]
--
-- tal que hamming' es la sucesión de Hamming. Por ejemplo,
--
-- take 12 hamming' == [1,2,3,4,5,6,8,9,10,12,15,16]

hamming' :: [Int]
hamming' = [x | x <- [1..], divisoresEn x [2,3,5]]

-- 7.15.4 - Definir la función
--
-- cantidadHammingMenores :: Int -> Int
--
-- tal que (cantidadHammingMenores x) es la cantidad de números de Hamming
-- menores que x. Por ejemplo,
--
-- cantidadHammingMenores 6 == 5
-- cantidadHammingMenores 7 == 6
-- cantidadHammingMenores 8 == 6

cantidadHammingMenores :: Int -> Int
cantidadHammingMenores x = length (takeWhile (<x) hamming')

-- 7.15.5 - Definir la función
--
-- siguienteHamming :: Int -> Int
--
-- tal que (siguienteHamming x) es el menor número de la sucesión de Hamming 
-- mayor que x. Por ejemplo,
--
-- siguienteHamming 6 == 8
-- siguienteHamming 21 == 24

siguienteHamming :: Int -> Int
siguienteHamming x = head (dropWhile (<=x) hamming')

-- 7.15.6 - Definir la función
--
-- huecoHamming :: Int -> [(Int,Int)]
--
-- tal que (huecoHamming n) es la lista de pares de números consecutivos en la 
-- sucesión de Hamming cuya distancia es mayor que n. Por ejemplo,
--
-- take 4 (huecoHamming 2) == [(12,15),(20,24),(27,30),(32,36)]
-- take 3 (huecoHamming 2) == [(12,15),(20,24),(27,30)]
-- take 2 (huecoHamming 3) == [(20,24),(32,36)]
-- head (huecoHamming 10) == (108,120)
-- head (huecoHamming 1000) == (34992,36000)

huecoHamming :: Int -> [(Int,Int)]
huecoHamming n = [(x,y) | x <- hamming',
                          let y = siguienteHamming x,
                          y-x > n]

-- 7.15.7 - Comprobar con QuickCheck que para todo n, existen pares de números
-- consecutivos en la sucesión de Hamming cuya distancia es mayor o igual que 
-- n.

prop_Hamming :: Int -> Bool
prop_Hamming n = huecoHamming n' /= []
                 where n' = abs n

-- La comprobación es
-- 
-- ghci> quickCheck prop_Hamming
-- OK, passed 100 tests.
--
-- 7.16 - Suma de los primos menores que n
-- -----------------------------------------------------------------------------
-- Problema 10 del proyecto Euler. Definir la función
--
-- sumaPrimoMenores :: Int -> Int
--
-- tal que (sumaPrimoMenores 10 n) es la suma de los primos menores que n. Por 
-- ejemplo,
--
-- sumaPrimoMenores 10 == 17

sumaPrimoMenores :: Int -> Int
sumaPrimoMenores n = sumaMenores n primos 0
   where sumaMenores n (x:xs) a | n <= x = a
                                | otherwise = sumaMenores n xs (a+x)

-- donde primos es la lista de los números primos definida previamente
--
-- 7.17 - Menor número triangular con más de n divisores
-- -----------------------------------------------------------------------------
-- Problema 12 del proyecto Euler. La sucesión de los números triangulares
-- se obtiene sumando los números naturales. Así, el 7º número triangular es
--
-- 1 + 2 + 3 + 4 + 5 + 6 + 7 = 28.
--
-- Los primeros 10 números triangulares son
--
-- 1, 3, 6, 10, 15, 21, 28, 36, 45, 55, . . .
--
-- Los divisores de los primeros 7 números triangulares son:
--
-- 1 : 1
-- 3 : 1, 3
-- 6 : 1, 2, 3, 6
-- 10 : 1, 2, 5, 10
-- 15 : 1, 3, 5, 15
-- 21 : 1, 3, 7, 21
-- 28 : 1, 2, 4, 7, 14, 28
--
-- Como sepuede observar, 28 es el número triangular con más de 5 divisores.
-- Definir la función
--
-- euler12 :: Int -> Integer
--
-- tal que (euler12 n) es el menor número triangular con más de n divisores.
-- Por ejemplo,
--
-- euler12 5 == 28

euler12 :: Int -> Integer
euler12 n = head [x | x <- triangulares, nDivisores x > n]

-- donde se usan las siguientes funciones auxiliares
--
-- * triangulares es la lista de los números triangulares
-- 
-- take 10 triangulares => [1,3,6,10,15,21,28,36,45,55]

triangulares :: [Integer]
triangulares = 1:[x+y | (x,y) <- zip [2..] triangulares]

-- Otra definición de triangulares es

triangulares' :: [Integer]
triangulares' = scanl (+) 1 [2..]

-- * (divisores n) es la lista de los divisores de n. Por ejemplo,
--
-- divisores 28 == [1,2,4,7,14,28]

divisores2 :: Integer -> [Integer]
divisores2 x = [y | y <- [1..x], mod x y == 0]

-- * (nDivisores n) es el número de los divisores de n. Por ejemplo,
--
-- nDivisores 28 == 6

nDivisores :: Integer -> Int
nDivisores x = length (divisores2 x)

-- 7.18 - Números primos consecutivos con dígitos con igual media
-- -----------------------------------------------------------------------------
-- Definir la función
--
-- primosEquivalentes :: Int -> [[Integer]]
--
-- tal que (primosEquivalentes n) es la lista de las sucesiones de n números
-- primos consecutivos con la media de sus dígitos iguales. Por ejemplo,
--
-- take 2 (primosEquivalentes 2) == [[523,541],[1069,1087]]
-- head (primosEquivalentes 3) == [22193,22229,22247]

primosEquivalentes :: Int -> [[Integer]]
primosEquivalentes n = aux primos
    where aux (x:xs) | relacionados equivalentes ys = ys : aux xs
                     | otherwise                    = aux xs
                     where ys = take n (x:xs)

-- Donde primos se ha definido previamente
-- (relacionados r xs) se define a continuación

relacionados :: (a -> a -> Bool) -> [a] -> Bool
relacionados r (x:y:zs) = (r x y) && relacionados r (y:zs)
relacionados _ _ = True

-- (equivalentes x y) se define a continuación

equivalentes :: Integer -> Integer -> Bool
equivalentes x y = media (digitosC x) == media (digitosC y)

-- (media xs) se define a continuación

media :: [Integer] -> Float
media xs = (fromIntegral (sum xs)) / (fromIntegral (length xs))

-- (digitosC n) se define a continuación

digitosC :: Integer -> [Integer]
digitosC n = [read [x] | x <- show n]

-- 7.19 - Decisión de pertenencia al rango de una función creciente
-- -----------------------------------------------------------------------------
-- Definir la función
--
-- perteneceRango :: Int -> (Int -> Int) -> Bool
--
-- tal que (perteneceRango x f) se verifica si x pertenece al rango de la 
-- función f, suponiendo que f es una función creciente cuyo dominio es el 
-- conjunto de los números naturales. Por ejemplo,
--
-- perteneceRango 5 (\x -> 2*x+1) == True
-- perteneceRango 1234 (\x -> 2*x+1) == False

perteneceRango :: Int -> (Int -> Int) -> Bool
perteneceRango y f = y `elem` takeWhile (<=y) (imagenes f)
    where imagenes f = [f x | x <- [0..]]

-- 7.20 - Pares ordenados por posición
-- -----------------------------------------------------------------------------
-- 7.20.1 - Definir, por recursión, la función
--
-- paresOrdenados :: [a] -> [(a,a)]
--
-- tal que (paresOrdenados xs) es la lista de todos los pares de elementos 
-- (x,y) de xs, tales que x ocurren en xs antes que y. Por ejemplo,
--
-- paresOrdenados [3,2,5,4] == [(3,2),(3,5),(3,4),(2,5),(2,4),(5,4)]
-- paresOrdenados [3,2,5,3] == [(3,2),(3,5),(3,3),(2,5),(2,3),(5,3)]

paresOrdenados :: [a] -> [(a,a)]
paresOrdenados []     = []
paresOrdenados (x:xs) = [(x,y) | y <- xs] ++ paresOrdenados xs

-- 7.20.2 - Definir, por plegado, la función
--
-- paresOrdenados2 :: [a] -> [(a,a)]
--
-- tal que (paresOrdenados2 xs) es la lista de todos los pares de elementos 
-- (x,y) de xs, tales que x ocurren en xs antes que y. Por ejemplo,
--
-- paresOrdenados2 [3,2,5,4] == [(3,2),(3,5),(3,4),(2,5),(2,4),(5,4)]
-- paresOrdenados2 [3,2,5,3] == [(3,2),(3,5),(3,3),(2,5),(2,3),(5,3)]

paresOrdenados2 :: [a] -> [(a,a)]
paresOrdenados2 [] = []
paresOrdenados2 (x:xs) =
    foldr (\y ac -> (x,y):ac) (paresOrdenados2 xs) xs

-- 7.20.3 - Definir, usando repeat, la función
--
-- paresOrdenados3 :: [a] -> [(a,a)]
--
-- tal que (paresOrdenados3 xs) es la lista de todos los pares de elementos 
-- (x,y) de xs, tales que x ocurren en xs antes que y. Por ejemplo,
--
-- paresOrdenados3 [3,2,5,4] == [(3,2),(3,5),(3,4),(2,5),(2,4),(5,4)]
-- paresOrdenados3 [3,2,5,3] == [(3,2),(3,5),(3,3),(2,5),(2,3),(5,3)]

paresOrdenados3 :: [a] -> [(a,a)]
paresOrdenados3 []     = []
paresOrdenados3 (x:xs) = zip (repeat x) xs ++ paresOrdenados3 xs

-- 7.21 - Aplicación iterada de una fucnión
-- -----------------------------------------------------------------------------
-- 7.21.1 - Definir, por recursión, la función
--
-- potenciaFunc :: Int -> (a -> a) -> a -> a
--
-- tal que (potenciaFunc n f x) es el resultado de aplicar n veces la función f
-- a x. Por ejemplo,
--
-- potenciaFunc 3 (*10) 5 == 5000
-- potenciaFunc 4 (+10) 5 == 45

potenciaFunc :: Int -> (a -> a) -> a -> a
potenciaFunc 0 _ x = x
potenciaFunc n f x = potenciaFunc (n-1) f (f x)

-- 7.21.2 - Definir, sin recursión, la función
--
-- potenciaFunc2 :: Int -> (a -> a) -> a -> a
--
-- tal que (potenciaFunc2 n f x) es el resultado de aplicar n veces la función f
-- a x. Por ejemplo,
--
-- potenciaFunc2 3 (*10) 5 == 5000
-- potenciaFunc2 4 (+10) 5 == 45

potenciaFunc2 :: Int -> (a -> a) -> a -> a
potenciaFunc2 n f x = last (take (n+1) (iterate f x))

-- 7.22 - Expresión de un número como suma de dos de una lista
-- -----------------------------------------------------------------------------
-- 7.22.1 - Definir, por recursión, la función
--
-- sumaDeDos :: Int -> [Int] -> Maybe (Int,Int)
--
-- tal que (sumaDeDos x ys) decide si x puede expresarse como suma de dos 
-- elementos de ys y, en su caso, devuelve un par de elementos de ys cuya suma 
-- es x. Por ejemplo,
--
-- sumaDeDos 9 [7,4,6,2,5] == Just (7,2)
-- sumaDeDos 5 [7,4,6,2,5] == Nothing

sumaDeDos :: Int -> [Int] -> Maybe (Int,Int)
sumaDeDos _ []  = Nothing
sumaDeDos _ [_] = Nothing
sumaDeDos y (x:xs) | y-x `elem` xs = Just (x,y-x)
                   | otherwise     = sumaDeDos y xs

-- 7.22.2 - Definir, usando la función paresOrdenados, definida antes, la 
-- función
--
-- sumaDeDos' :: Int -> [Int] -> Maybe (Int,Int)
--
-- tal que (sumaDeDos' x ys) decide si x puede expresarse como suma de dos 
-- elementos de ys y, en su caso, devuelve un par de elementos de ys cuya suma 
-- es x. Por ejemplo,
--
-- sumaDeDos' 9 [7,4,6,2,5] == Just (7,2)
-- sumaDeDos' 5 [7,4,6,2,5] == Nothing

sumaDeDos' :: Int -> [Int] -> Maybe (Int,Int)
sumaDeDos' x xs
    | null ys   = Nothing
    | otherwise = Just (head ys)
    where ys = [(a,b) | (a,b) <- paresOrdenados xs , a+b == x]
