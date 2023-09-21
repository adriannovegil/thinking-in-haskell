module Recursion where

import Test.QuickCheck

main :: IO ()
main = return ()

-- 3.1 - Potencia de exponente natural
-- -----------------------------------------------------------------------------
-- Definir, por recursión la función
--
-- potencia :: Integer -> Integer -> Integer
--
-- tal que (potencia x n) es x elevado al número natural n. Por ejemplo,
--
-- potencia 2 3 == 8

potencia :: Integer -> Integer -> Integer
potencia m 0 = 1
potencia m n = m*(potencia m (n-1))

-- 3.2 - Replicación de un elemento
-- -----------------------------------------------------------------------------
-- Definir por recursión la función
--
-- replicate' :: Int -> a -> [a]
-- 
-- tal que (replicate' n x) es la lista formado por n copias del elemento x. Por
-- ejemplo,
--
-- replicate' 3 2 = [2,2,2]

replicate' :: Int -> a -> [a]
replicate' 0 _ = []
replicate' n x = x : replicate' (n-1) x

-- 3.3 - Doble factorial
-- -----------------------------------------------------------------------------
-- El doble factorial de un número n se define por
--
-- 0!! = 1
-- 1!! = 1
-- n!! = n*(n-2)* ... * 3 * 1, si n es impar
-- n!! = n*(n-2)* ... * 4 * 2, si n es par
--
-- Por ejemplo,
--
-- 8!! = 8*6*4*2 = 384
-- 9!! = 9*7*5*3*1 = 945
--
-- Definir por recursión, la función
--
-- dobleFactorial :: Integer -> Integer
--
-- tal que (dobleFactorial n) es el doble factorial de n. Por ejemplo,
--
-- dobleFactorial 8 == 384
-- doblefactorial 9 == 945

dobleFactorial :: Integer -> Integer
dobleFactorial 0 = 1
dobleFactorial 1 = 1
dobleFactorial n = n * dobleFactorial (n-2)

-- 3.4 - Algoritmo de Euclides del máximo común divisor
-- -----------------------------------------------------------------------------
-- Dados dos números naturales, a y b, es posible calcular su máximo común 
-- divisor mediante el Algoritmo de Euclides. Este algoritmo se puede resumir en
-- la siguiente fórmula:
--
-- mcd(a,b) = a,                  si b = 0
--            mcd(b, a módulo b), si b > 0
--
-- Definir la función
--
-- mcd :: Integer -> Integer -> Integer
-- 
-- tal que (mcd a b) es el máximo común divisor de a y b calculado mediante el 
-- algoritmo de Euclides. Por ejemplo, 
--
-- mcd 30 45 == 15

mcd :: Integer -> Integer -> Integer
mcd a 0 = a
mcd a b = mcd b (a `mod` b)

-- 3.5 - Menor núero divisible por una sucesión de números
-- -----------------------------------------------------------------------------
-- Los siguientes ejercicios tienen como objetivo resolver el problema 5 del 
-- proyecto Euler que consiste en calcular el menor número divisible por los 
-- números del 1 al 20
-- 
-- 3.5.1 - Definir por recursión la función
--
-- menorDivisible :: Integer -> Integer -> Integer
--
-- tal que (menosDivisible a b) es el menor número divisible por los número del
-- a al b. Por ejemplo,
--
-- menorDivisible 2 5 = 60
--
-- Indicación: Usar la función lcm tal que (lcm x y) es el mínimo común 
-- múltiplo de x e y.

menorDivisible :: Integer -> Integer -> Integer
menorDivisible a b
    | a == b    = a
    | otherwise = lcm a (menorDivisible (a+1) b)

-- 3.5.2 - Definir la constante
--
-- euler5 :: Integer
--
-- tal que euler5 es el menor número divisible por los números de 1 al 20 y 
-- calcular su valor

euler5 :: Integer
euler5 = menorDivisible 1 20

-- El cálculo es
--
-- ghci> euler5
-- 232792560
--
-- 3.6 - Número de pasos para resolver el problema de las torres de Hanoi
-- 
-- En un templo hindú se encuentran tres varillas de platino. En una de ellas, 
-- hay 64 anillos de oro de distintos radios, colocados de mayor a menor.
-- El trabajo de los monjes de ese templo consiste en pasarlos todos a la 
-- tercera varilla, usando la segunda como varilla auxiliar, con las siguientes
-- condiciones
--
-- 1) En cada paso sólo se puede mover un anillo.
-- 2) Nunca puede haber un anillo de mayor diámetro encima de uno de menor 
--    diámetro.
--
-- La leyenda dice que cuanod todos los anillos se encuentren en la tercera
-- varilla, será el fin del mundo.
--
-- Definir la función
--
-- numPasoHanoi :: Integer -> Integer
--
-- tal que (numPasoHanoi n) es el núero de pasos necesarios para trasladar n
-- anillos. Por ejemplo,
--
-- numPasosHanoi 2 == 3
-- numPasosHanoi 7 == 127
-- numPasosHanoi 64 == 18446744073709551615
-- 
-- Sean A, B y C las tres varillas. La estrategia recursiva es la siguiente: 
--
-- - Caso base (n = 1): Se mueve el disco de A a C
-- - Caso inductivo (n = m + 1): Se mueven m discos de A a C. Se mueve el disco
--   de A a B. Se mueven m discos de C a B
--
-- Por tanto,

numPasosHanoi :: Integer -> Integer
numPasosHanoi 1 = 1
numPasosHanoi n = 1 + 2 * numPasosHanoi (n-1)

-- 3.7 - Conjunción de una lista
-- -----------------------------------------------------------------------------
-- Definir por recursión la función
--
-- and' :: [Bool] -> Bool
--
-- tal que (and' xs) se verifica si todos los elemntos de xs son verdadero. Por 
-- ejemplo, 
--
-- and' [1+2 < 4, 2:[3] == [2,3]] == True
-- and' [1+2 < 3, 2:[3] == [2,3]] == False

and' :: [Bool] -> Bool
and' [] = True
and' (b:bs) = b && and' bs

-- 3.8 - Pertenencia a una lista
-- -----------------------------------------------------------------------------
-- Definir por recursión la función
--
-- elem' :: Eq a => a -> [a] -> Bool
--
-- tal que (elem' x xs) se verifica si x pertenece a la lista xs. Por ejemplo,
--
-- elem' 3 [2,3,5] == True
-- elem' 4 [2,3,5] == False

elem' :: Eq a => a -> [a] -> Bool
elem' x []                 = False
elem' x (y:ys) | x == y    = True
               | otherwise = elem' x ys

-- 3.9 - Último elemento de una lista
-- -----------------------------------------------------------------------------
-- Definir or recursión la función
--
-- last'::[a] -> a
--
-- tal que (last xs) es el último elemento de xs. Por ejemplo,
--
-- last' [2,3,4] => 5

last' :: [a] -> a
last' [x]    = x
last' (_:xs) = last' xs

-- 3.10 - Concatenación de una lista
-- -----------------------------------------------------------------------------
-- Definir por recursión la función
--
-- concat' :: [[a]] -> [a]
--
-- tal que (concat' xss) es la lista obtenida concatenando las listas de xss. 
-- Por ejemplo,
--
-- concat' [[1..3],[5..7],[8..10]] == [1,2,3,5,6,7,8,9,10]

concat' :: [[a]] -> [a]
concat' []       = []
concat' (xs:xss) = xs ++ concat' xss

-- 3.11 - Selección de un elemento
-- -----------------------------------------------------------------------------
-- Definir por recursión la función
--
-- selecciona :: [a] -> Int -> a
--
-- tal que (selecciona xs n) es el n-ésimo elemento de xs. Por ejemplo,
--
-- selecciona [2,3,5,7] 2 == 5

selecciona :: [a] -> Int -> a
selecciona (x:_) 0  = x
selecciona (_:xs) n = selecciona xs (n-1)

-- 3.12 - Selección de los primeros elementos
-- -----------------------------------------------------------------------------
-- Definir por recursión la funición
--
-- take' :: Int -> [a] -> [a]
--
-- tal que (take' n xs) es la lista de los n primeros elementos de xs. Por ejemplo,
--
-- take' 3 [4..12] == [4,5,6]

take' :: Int -> [a] -> [a]
take' 0 _      = []
take' _ []     = []
take' n (x:xs) = x : take' (n-1) xs

-- 3.13 - Intercalación de la media aritmética
-- -----------------------------------------------------------------------------
-- Definir por recursión la función
-- refina :: [Float] -> [Float]
--
-- tal que (refina xs) es la lista obtenida intercalando entre cada dos 
-- elementos consecutivos de xs su media aritmética. Por ejemplo, 
--
-- refinada [2,7,1,8] == [2.0,4.5,7.0,4.0,1.0,4.5,8.0]
-- refinada [2]       == [2.0]
-- refinada []        == []

refinada :: [Float] -> [Float]
refinada (x:y:zs) = x : (x+y)/2 : refinada (y:zs)
refinada xs       = xs

-- 3.14 - Ordenación por mezcla
-- -----------------------------------------------------------------------------
-- 3.14.1 - Mezcla de listas ordenadas
-- Definir por recursión la función
--
-- mezcla :: Ord a => [a] -> [a] -> [a]
--
-- tal que (mezcla xs ys) es la lista obtenida mezclando las listas ordenadas 
-- de xs e ys. Por ejemplo,
--
-- mezcla [2,5,6] [1,3,4] == [1,2,3,4,5,6]

mezcla :: Ord a => [a] -> [a] -> [a]
mezcla []     ys                 = ys
mezcla xs     []                 = xs
mezcla (x:xs) (y:ys) | x <= y    = x : mezcla xs (y:ys)
                     | otherwise = y : mezcla (x:xs) ys

-- 3.14.2 - Mitades de una lista
-- Definir la función
--
-- mitades :: [a] -> ([a],[a])
--
-- tal que (mitades xs) es el par formado por las dos mitades en que se divide 
-- xs tales que sus longitudes difieren como máximo en uno. Por ejemplo,
-- 
-- mitades [2,3,5,7,9] == ([2,3],[5,7,9])

mitades :: [a] -> ([a],[a])
mitades xs = splitAt (length xs `div` 2) xs

-- 3.14.3 - Ordenación por mezcla
--
-- Definir por recursión la funición
-- 
-- ordMezcla :: Ord a => [a] -> [a]
--
-- tal que (ordMezcla xs) es la lista obtenida ordenando xs por mezcla (es 
-- decir, considerando que la lista vacía y las listas unitarias están 
-- ordenadas y cualquier otra lista se ordena mezclando las dos listas que 
-- resultan de ordenar sus dos mitades por separado). Por ejemplo,
--
-- ordMezcla [5,2,3,1,7,2,5] == [1,2,2,3,5,5,7]

ordMezcla :: Ord a => [a] -> [a]
ordMezcla []  = []
ordMezcla [x] = [x]
ordMezcla xs  = mezcla (ordMezcla ys) (ordMezcla zs)
                where (ys,zs) = mitades xs

-- 3.14.4 - La ordenación por mezcla de listas ordenadas
-- Definir por recursión la función
--
-- ordenada :: Ord a => [a] -> Bool
--
-- tal que (ordenada xs) se verifica si xs es una lista ordenada. Por ejemplo,
--
-- ordenada [2,3,5] == True
-- ordenada [2,5,3] == False

ordenada :: Ord a => [a] -> Bool
ordenada []       = True
ordenada [_]      = True
ordenada (x:y:xs) = x <= y && ordenada (y:xs)

-- 3.14.5 - Comprobar con QuickCheck que la ordenación por mezcla de una lista
-- es una lista ordenada.

prop_ordMezcla_ordenada :: Ord a => [a] -> Bool
prop_ordMezcla_ordenada xs = ordenada (ordMezcla xs)

-- La comprobación es
--
-- ghci> quickCheck prop_ordMezcla_ordenada
-- +++ OK, passed 100 tests.
--
-- 3.14.6 - Ordenación por Mezcla de una permutación
-- Definir por recursión la función
--
-- borra :: Eq a => a -> [a] -> [a]
--
-- tal que (borra x xs) es la lista obtenida borrando una ocurrencia de x en 
-- la lista xs. Por ejemplo,
--
-- borra 1 [1,2,1] == [2,1]
-- borra 3 [1,2,1] == [1,2,1]

borra :: Eq a => a -> [a] -> [a]
borra x [] = []
borra x (y:ys) | x == y = ys
               | otherwise = y : borra x ys

-- 3.14.7 - Determinación de permutaciones
-- Definir por recursión la función
--
-- esPermutación :: Eq a => [a] -> [a] -> Bool
--
-- tal que (esPermutacion xs ys) se verifica si xs es una permutación de ys. 
-- Por ejemplo,
--
-- esPermutacion [1,2,1] [2,1,1] == True
-- esPermutacion [1,2,1] [1,2,2] == False

esPermutacion :: Eq a => [a] -> [a] -> Bool
esPermutacion []     []     = True
esPermutacion []     (y:ys) = False
esPermutacion (x:xs) ys     = elem x ys && esPermutacion xs (borra x ys)

-- 3.14.8 - Comrpobar con QuieckCheck que la ordenación por mezcla de una lista
-- es una permutación de la lista.

prop_ordMezcla_pemutacion :: Ord a => [a] -> Bool
prop_ordMezcla_pemutacion xs = esPermutacion (ordMezcla xs) xs

-- La comprobación es
--
-- ghci> quickCheck prop_ordMezcla_permutacion
-- +++ OK, passed 100 tests.