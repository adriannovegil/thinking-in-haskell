module Recursion where

import Data.List
import Test.QuickCheck

main :: IO ()
main = return ()

-- 4.1 - Suma de los cuadrados de los primeros números
-- -----------------------------------------------------------------------------
-- 4.1.1 - Definir, por recursión, la función
--
-- sumaCuadradosR :: Integer -> Integer
--
-- tal que (sumaCuadradosR n) es la suma de los cuadrados de los número de 1 a 
-- n. Por ejemplo,
--
-- sumaCuadradosR 4 == 30

sumaCuadradosR :: Integer -> Integer
sumaCuadradosR 0 = 0
sumaCuadradosR n = n^2 + sumaCuadradosR (n-1)

-- 4.1.2 - Comprobar con QuickCheck si (sumaCuadradosR n) es igual a 
-- (n(n+1)(2n+1))/6.

prop_SumaCuadrados n =
  n >= 0 ==>
    sumaCuadradosR n == n * (n+1) * (2*n+1) `div` 6

-- La comrpobación es
--
-- ghci> quickCheck prop_SumaCuadrados
-- +++ OK, passed 100 tests.
--
-- 4.1.3 - Definir, por comprensión, la función
--
-- sumaCuadradosC :: Integer -> Integer
-- 
-- tal que (sumaCuadradosC n) es la suma de los cuadrados de los números de 1 a
--  n. Por ejemplo,
--
-- sumaCuadradosC 4 == 30

sumaCuadradosC :: Integer -> Integer
sumaCuadradosC n = sum [x^2 | x <- [1..n]]

-- 4.1.4 - Comprobar con QuickCheck que las funciones sumaCuadradosR y 
-- sumaCuadradosC son equivalentes sobre los números naturales.

prop_sumaCuadrados n =
    n >= 0 ==> sumaCuadradosR n == sumaCuadradosC n

-- La comrpobación es
--
-- ghci> quickCheck prop_sumaCuadrados
-- +++ OK, passed 100 tests.
--
-- 4.2 - Número de bloques de escaleras triangulares
-- -----------------------------------------------------------------------------
-- Se quiere formar una escalera con bloques cuadrados, de forma que tenga un 
-- número determinado de escalones. Por ejemplo, una escalera con tres escalones
-- tendría la siguiente forma:
--
--     XX
--   XXXX
-- XXXXXX
--
-- 4.2.1 - Definir, por recursión, la función
--
-- numeroBloquesR :: Integer -> Integer
--
-- tal que (numeroBloquesR n) es el número de bloques necesarios par construir
-- una escalera con n escalones. Por ejemplo,
--
-- numeroBloquesR 1 == 2
-- numeroBloquesR 3 == 12
-- numeroBloquesR 10 == 110

numeroBloquesR :: Integer -> Integer
numeroBloquesR 0 = 0
numeroBloquesR n = 2*n + numeroBloquesR (n-1)

-- 4.2.2 - Definir, por comprensión, la función
--
-- numeroBloquesC :: Integer -> Integer
-- 
-- tal que (numeroBloquesC n) es el número de bloques necesarios para 
-- construir una escalera con n escalones. Por ejemplo,
--
-- numeroBloquesC 1 == 2
-- numeroBloquesC 3 == 12
-- numeroBloquesC 10 == 110

numeroBloquesC :: Integer -> Integer
numeroBloquesC n = sum [2*x | x <- [1..n]]

-- 4.2.3 - Comprobar con QuickCheck que (numeroBloquesC n) es igual a n + n^2.

prop_numeroBloques n =
    n > 0 ==> numeroBloquesC n == n+n^2

-- La comrpobación es
--
-- ghci> quickCheck prop_numeroBloques
-- +++ OK, passed 100 tests.
--
-- 4.3 - Suma de los cuadrados de los impares entre los primeros números
-- -----------------------------------------------------------------------------
-- 4.3.1 - Definir, por recursión, la función
--
-- sumaCuadradosImparesR :: Integer -> Integer
-- 
-- tal que (sumaCuadradosImparesR n) es la suma de los cuadrados de los números
-- impares desde 1 hasta n. Por ejemplo,
--
-- sumaCuadradosImparesR 1 == 1
-- sumaCuadradosImparesR 7 == 84
-- sumaCuadradosImparesR 4 == 10

sumaCuadradosImparesR :: Integer -> Integer
sumaCuadradosImparesR 1 = 1
sumaCuadradosImparesR n
    | odd n = n^2 + sumaCuadradosImparesR (n-1)
    | otherwise = sumaCuadradosImparesR (n-1)

-- 4.3.2 - Definir, por comprensión, la función
--
-- sumaCuadradosImparesC :: Integer -> Integer
--
-- tal que (sumaCuadradosImparesC n) es la suma de los cuadrados de los 
-- números impares desde 1 hasta n. Por ejemplo,
--
-- sumaCuadradosImparesC 1 == 1
-- sumaCuadradosImparesC 7 == 84
-- sumaCuadradosImparesC 4 == 10

sumaCuadradosImparesC :: Integer -> Integer
sumaCuadradosImparesC n = sum [x^2 | x <- [1..n], odd x]

-- Otra definición más simple es

sumaCuadradosImparesC' :: Integer -> Integer
sumaCuadradosImparesC' n = sum [x^2 | x <- [1,3..n]]

-- 4.4 - Operaciones con los dígitos de los núnermos
-- -----------------------------------------------------------------------------
-- 4.4.1 - Lista de los dígitos de un número
-- 4.4.1.1 - Definir, por recursión, la función
-- 
-- digitosR :: Integer -> [Int]
--
-- tal que (digitosR n) es la lista de los dígitos del número n. Por ejemplo,
--
-- digitosR 320274 == [3,2,0,2,7,4]

digitosR :: Integer -> [Integer]
digitosR n = reverse (digitosR' n)

digitosR' n
    | n < 10 = [n]
    | otherwise = (n `rem` 10) : digitosR' (n `div` 10)

-- 4.4.1.2 - Definir, por comprensión, la función
-- 
-- digitosC :: Integer -> [Int]
--
-- tal que (digitasC n) es la lista de los dígitos del número n. Por ejemplo,
--
-- digitosC 320274 == [3,2,0,2,7,4]
--
-- Indicación: Usar las funciones shoy y read

digitosC :: Integer -> [Integer]
digitosC n = [read [x] | x <- show n]

-- 4.4.1.3 - Comprobar con QuickCheck que las funciones digitosR y digitos son 
-- equivalentes.

prop_digitos n =
    n >= 0 ==>
    digitosR n == digitosC n

-- La comprobación es
-- 
-- ghci> quickCheck prop_digitos
-- +++ OK, passed 100 tests.

-- 4.4.2 - Suma de los dígitos de un número
-- 4.4.2.1 - Definir, por recursión, la función
--
-- sumaDigitosR :: Integer -> Integer
--
-- tal que (sumaDigitosR n) es la suma de los dígitos de n. Por ejmplo,
-- 
-- sumaDigitosR 3     == 3
-- sumaDigitosR 2454  == 15
-- sumaDigitosR 20045 == 11

sumaDigitosR :: Integer -> Integer
sumaDigitosR n
    | n < 10 = n
    | otherwise = n `rem` 10 + sumaDigitosR (n `div` 10)

-- 4.4.2.2 - Definir, sin usar recursión, la función
--
-- sumaDigitosNR :: Integer -> Integer
-- 
-- tal que (sumaDigitosNR n) es la suma de los dígitos de n. Por ejemplo,
--
-- sumaDigitosNR 3 == 3
-- sumaDigitosNR 2454 == 15
-- sumaDigitosNR 20045 == 11

sumaDigitosNR :: Integer -> Integer
sumaDigitosNR n = sum (digitosR n)

-- 4.4.2.3 - Comprobar con QuickCheck que las funciones sumaDigitosR y sumaDigitosNR son
-- equivalentes.

prop_sumaDigitos n =
    n >= 0 ==>
    sumaDigitosR n == sumaDigitosNR n

-- La comprobación es
-- 
-- ghci> quickCheck prop_sumaDigitos
-- +++ OK, passed 100 tests.
--
-- 4.4.3 - Decidir si es un dígito del número
-- Definir la función
--
-- esDigito :: Integer -> Integer -> Bool
--
-- tal que (esDigito x n) se verifica si x es un dígito de n. Por ejemplo,
-- 
-- esDigito 4 1041 == True
-- esDigito 3 1041 == False

esDigito :: Integer -> Integer -> Bool
esDigito x n = elem x (digitosR n)

-- 4.4.4 - Número de dígitos de un número
-- Definir la función
--
-- numeroDeDigitos :: Integer -> Integer
--
-- tal que (numeroDeDigitos x) es el número de dígitos de x. Por ejemplo,
--
-- numeroDeDigitos 34047 == 5

numeroDeDigitos :: Integer -> Int
numeroDeDigitos x = length (digitosR x)

-- 4.4.5 - Número correspondiente a una lista de dígitos
-- 4.4.5.1 - Definir, por recursión, la función
-- 
-- listaNumeroR :: [Integer] -> Integer
--
-- listaNumeroR [5]       == 5
-- listaNumeroR [1,3,4,7] == 1347
-- listaNumeroR [0,0,1]   == 1

listaNumeroR :: [Integer] -> Integer
listaNumeroR xs = listaNumeroR' (reverse xs)

listaNumeroR' :: [Integer] -> Integer
listaNumeroR' [x]    = x
listaNumeroR' (x:xs) = x + 10 * (listaNumeroR' xs)

-- 4.4.5.2 - Definir, por comprensión, la función
--
-- listaNumeroC :: [Integer] -> Integer
--
-- tal que (listaNumeroC xs) es el número formado por los dígitos de la lista 
-- xs. Por ejemplo,
--
-- listaNumeroC [5]       == 5
-- listaNumeroC [1,3,4,7] == 1347
-- listaNumeroC [0,0,1]   == 1

listaNumeroC :: [Integer] -> Integer
listaNumeroC xs = sum [y*10^n | (y,n) <- zip (reverse xs) [0..]]

-- 4.4.6 - Concatenación de los números
-- 4.4.6.1 - Definir, por recursión, la función
--
-- pegaNumerosR :: Integer -> Integer -> Integer
--
-- tal que (pegaNumerosR x y) es el número resultante de "pegar" los número x 
-- e y. Por ejemplo,
--
-- pegaNumerosR 12 987 == 12987
-- pegaNumerosR 1204 7 == 12047
-- pegaNumerosR 100 100 == 100100

pegaNumerosR :: Integer -> Integer -> Integer
pegaNumerosR x y
    | y < 10 = 10*x+y
    | otherwise = 10 * pegaNumerosR x (y `div`10) + (y `rem` 10)

-- 4.4.6.2 - Definir, sin usar recursión, la función
-- 
-- pegaNumerosNR :: Integer -> Integer -> Integer
--
-- tal que (pegaNumerosNR x y) es el número resultante de "pegar" los números x 
-- e y. Por ejemplo,
-- 
-- pegaNumerosNR 12 987 == 12987
-- pegaNumerosNR 1204 7 == 12047
-- pegaNumerosNR 100 100 == 100100

pegaNumerosNR :: Integer -> Integer -> Integer
pegaNumerosNR x y = listaNumeroC (digitosR x ++ digitosR y)

-- 4.4.6.3 - Comprobar con QuickCheck que las funciones pegaNumerosR y 
-- pegaNumerosNR son equivalentes.

prop_pegaNumeros x y =
    x >= 0 && y >= 0 ==>
    pegaNumerosR x y == pegaNumerosNR x y

-- La comrpobación es
--
-- ghci> quickCheck prop_pegaNumeros
-- +++ OK, passed 100 tests.
--
-- 4.4.7 - Primer dígito de un número
-- 4.4.7.1 - Definir, por recursión, la función
--
-- primerDigitoR :: Integer -> Integer
--
-- tal que (primerDigitoR n) es el primer dígito de n. Por ejemplo,
--
-- primerDigitoR 425 == 4

primerDigitoR :: Integer -> Integer
primerDigitoR n
    | n < 10 = n
    | otherwise = primerDigitoR (n `div` 10)

-- 4.4.7.2 - Definir, sin usar recursión, la función
--
-- primerDigitoNR :: Integer -> Integer
--
-- tal que (primeroDigitoNR n) es el primer dígito de n. Por ejemplo,
--
-- primerDigitoNR 425 == 4

primerDigitoNR :: Integer -> Integer
primerDigitoNR n = head (digitosR n)

-- 4.4.7.3 - Comprobar con QuickCheck que las funciones primerDigitoR y 
-- primerDigitoNR son equivalentes.

prop_primerDigito x =
    x >= 0 ==>
    primerDigitoR x == primerDigitoNR x

-- La comprobación es
-- 
-- ghci> quickCheck prop_primerDigito
-- +++ OK, passed 100 tests.
--
-- 4.4.8 - Último dígito de un número
-- Definir la función
-- 
-- ultimoDigito :: Integer -> Integer
--
-- tal que (ultimoDigito n) es el último dígito de n. Por ejemplo,
--
-- ultimoDigito 425 == 5

ultimoDigito :: Integer -> Integer
ultimoDigito n = n `rem` 10

-- 4.4.9 - Número con los dígitos invertidos
-- 4.4.9.1 - Definir la función
--
-- inverso :: Integer -> Integer
-- 
-- tal que (inverso n) es el número obtenido escribiendo los dígitos de n en 
-- orden inverso. Por ejemplo,
--
-- inverso 42578 == 87524
-- inverso 203   == 302

inverso :: Integer -> Integer
inverso n = listaNumeroC (reverse (digitosR n))

-- 4.4.9.2 - Definir, usando show y read, la funicón
--
-- inverso' :: Integer -> Integer
-- 
-- tal que (inverso' n) es el número obtenido escribiendo los dígitos de n en 
-- orden inverso'. Por ejemplo,
--
-- inverso' 42578 == 87524
-- inverso' 203 == 302

inverso' :: Integer -> Integer
inverso' n = read (reverse (show n))

-- 4.4.9.3 - Comrpobar con QuickCheck que las funciones inverso e inverso' son 
-- equivalentes.

prop_inverso n =
    n >= 0 ==>
    inverso n == inverso' n

-- La comrpobación es
--
-- ghci> quickCheck prop_inverso
-- +++ OK, passed 100 tests.
--
-- 4.4.10 - Decidir si un número es capicúa
-- Definir la función
--
-- capicua :: Integer -> Bool
-- 
-- tal que (capicua n) se verifica si los dígitos de n son los mismos de 
-- izquierda a derecha que de derecha a izquierda. Por ejemplo,
--
-- capicua 1234 = False
-- capicua 1221 = True
-- capicua 4 = True

capicua :: Integer -> Bool
capicua n = n == inverso n

-- 4.4.11 - Suma de los dígitos de 2^1000
-- 4.4.11.1 - Definir la función
-- 
-- euler16 :: Integer -> Integer
-- 
-- tal que (euler16 n) es la suma de los dígitos de 2^n. Por ejemplo,
--
-- auler16 4 == 7

euler16 :: Integer -> Integer
euler16 n = sumaDigitosNR (2^n)

-- 4.4.11.2 - Calcular la suma de los dígitos de 2^1000.
-- El cálculo es
--
-- ghci> euler16 1000
-- 1366
--
-- 4.4.12 - Primitivo de un número
--
-- En el enunciado de uno de los problemas de las Olimpiadas matemáticas de 
-- Brasil se define el primitivo de un número como sigue:
--
-- Dado un número natural n, multiplicamos todos sus dígitos, repetimos este 
-- procedimiento hasta que quede un solo dígito al cual llamamos primitivo de 
-- n. Por ejemplo para 327 : 3 x 2 x 7 = 42 y 4 x 2 = 8. Por lo tanto, el 
-- primitivo de 327 es 8.
--
-- Definir la función
--
-- primitivo :: Integer -> Integer
--
-- tal que (primitivo n) es el primitivo de n. Por ejemplo.
--
-- primitivo 327 == 8

primitivo :: Integer -> Integer
primitivo n | n < 10 = n
            | otherwise = primitivo (producto n)

-- donde (producto n) es el producto de los dígitos de n. Por ejemplo,
--
-- producto 327 == 42

producto :: Integer -> Integer
producto = product . digitosC

-- 4.4.13 - Número con igual media de sus dígitos
-- Dos números son equivalentes si la media de sus dígitos son iguales. Por 
-- ejemplo, 3205 y 41 son equivalentes ya que
--
--                    3 + 4 + 0 + 5   4 + 1
--                    ------------- = -----
--                          4           2
--
-- Definir la función
-- 
-- equivalentes :: Int -> Int -> Bool
--
-- tal que (equivalentes x y) se verifica si los números x e y son equivalentes. Por ejemplo,
--
-- equivalentes 3205 41 == True
-- equivalentes 3205 25 == False

equivalentes :: Integer -> Integer -> Bool
equivalentes x y = media (digitosC x) == media (digitosC y)

-- donde (media xs) es la media de la lista xs. Por ejemplo,
--
-- media [3,2,0,5] == 2.5

media :: [Integer] -> Float
media xs = (fromIntegral (sum xs)) / (fromIntegral (length xs))

-- 4.4.14 - Números con dígitos duplicados en su cuadrado
-- Un número x es especial si el número de ocurrencia de cada dígito d de x en 
-- x^2 es el doble del número de ocurrencia de d en x. Por ejemplo, 72576 es 
-- especial por que tiene un 2, un 5, un 6 y dos 7 y su cuadrado es 5267275776
-- que tiene exactamente dos 2, dos 5, dos 6 y cuatro 7.
-- Definir la función
--
-- especial :: Integer -> Bool
--
-- tal que (especial x) se verifica si x es un número especial. Por ejemplo,
--
-- especial 72576 == True
-- especial 12 == False
--
-- Calcular el menor número especial mayor que 72576.

especial :: Integer -> Bool
especial x =
    sort (ys ++ ys) == sort (show (x^2))
    where ys = show x

-- El cálculo es
-- ghci> head [x | x <- [72577..], especial x]
-- 406512

-- 4.5 - Cuadrados de los elementos de una lista
-- -----------------------------------------------------------------------------
-- 4.5.1 - Definir, por comprensión, la función
--
-- cuadradosC :: [Integer] -> [Integer]
--
-- tal que (cuadradosC xs) es la lista de los cuadrados de xs. Por ejemplo,
-- 
-- cuadradosC [1,2,3] == [1,4,9]

cuadradosC :: [Integer] -> [Integer]
cuadradosC xs = [x*x | x <- xs]

-- 4.5.2 - Definir, por recursión, la función
--
-- cuadradosR :: [Integer] -> [Integer]
--
-- tal que (cuadradosR xs) es la lista de los cuadrados de xs. Por ejemplo,
--
-- cuadradosR [1,2,3] == [1,4,9]

cuadradosR :: [Integer] -> [Integer]
cuadradosR []     = []
cuadradosR (x:xs) = x*x : cuadradosR xs

-- 4.5.3 - Comprobar con QuickCheck que ambas definiciones son equivalentes.

prop_cuadrados :: [Integer] -> Bool
prop_cuadrados xs =
    cuadradosC xs == cuadradosR xs

-- La comrpobación es
--
-- ghci> quickCheck prop_cuadrados
-- +++ OK, passed 100 tests.
--
-- 4.6 - Números impares de una lista
-- -----------------------------------------------------------------------------
-- 4.6.1 - Definir, por comprensión, la función
--
-- imparesC :: [Integer] -> [Integer]
--
-- tal que (imparesC xs) es la lista de los números impares de xs. Por ejemplo,
--
-- imparesC [1,2,4,3,6] == [1,3]

imparesC :: [Integer] -> [Integer]
imparesC xs = [x | x <- xs, odd x]

-- 4.6.2 - Definir, por recursión, la función
--
-- imparesR :: [Integer] -> [Integer]
--
-- tal que (imparesR xs) es la lista de los números impares de xs. Por ejemplo,
--
-- imparesC [1,2,4,3,6] == [1,3]

imparesR :: [Integer] -> [Integer]
imparesR [] = []
imparesR (x:xs) | odd x = x : imparesR xs
                | otherwise = imparesR xs

-- 4.6.3 - Comprobar con QuichCheck que ambas definiciones son equivalentes.

prop_impares :: [Integer] -> Bool
prop_impares xs =
    imparesC xs == imparesR xs

-- La comrpobación es
--
-- ghci> quickCheck prop_impares
-- +++ OK, passed 100 tests.
--
-- 4.7 - Cuadrados de los elementos impares
-- -----------------------------------------------------------------------------
-- 4.7.1 - Definir, por comprensión, la función
--
-- imparesCuadradosC :: [Integer] -> [Integer]
--
-- tal que (imparesCuadradosC xs) es la lista de los cuadrados de los números 
-- impares de xs. Por ejemplo, 
--
--  imparesCuadradosC [1,2,4,3,6] == [1,9]

imparesCuadradosC :: [Integer] -> [Integer]
imparesCuadradosC xs = [x*x | x <- xs, odd x]

-- 4.7.2 - Definir, por recursión, la función
--
-- imparesCuadradosR :: [Integer] -> [Integer]
--
-- tal que (imparesCuadradosR xs) es la lista de los cuadrados de los números 
-- impares de xs. POr ejemplo,
--
--  imparesCuadradosR [1,2,4,3,6] == [1,9]

imparesCuadradosR :: [Integer] -> [Integer]
imparesCuadradosR [] = []
imparesCuadradosR (x:xs) | odd x = x*x : imparesCuadradosR xs
                         | otherwise = imparesCuadradosR xs

-- 4.7.3 - Comprobar con QuickCheck que ambas definiciones son equivalentes.

prop_imparesCuadrados :: [Integer] -> Bool
prop_imparesCuadrados xs =
    imparesCuadradosC xs == imparesCuadradosR xs

-- La comrpobación es
--
-- ghci> quickCheck prop_imparesCuadrados
-- +++ OK, passed 100 tests.
--
-- 4.8 - Suma de los cuadrados de los elementos impares
-- -----------------------------------------------------------------------------
-- 4.8.1 - Definir, por comprensión, la función
--
-- sumaCuadradosImparesC'' :: [Integer] -> Integer
--
-- tal que (sumaCuadradosImparesC'' xs) es la suma de los cuadrados de los 
-- números impares de la lista xs. Por ejemplo,
--
-- sumaCuadradosImparesC'' [1,2,4,3,6] == 10

sumaCuadradosImparesC'' :: [Integer] -> Integer
sumaCuadradosImparesC'' xs = sum [ x*x | x <- xs, odd x ]

-- 4.8.2 - Definir, por recursión, la función
--
-- sumaCuadradosImparesR :: [Integer] -> Integer
--
-- tal que (sumaCuadradosImparesR xs) es la suma de los cuadrados de los 
-- números impares de la lista xs. Por ejemplo,
--
-- sumaCuadradosImparesR [1,2,4,3,6] == 10

sumaCuadradosImparesR'' :: [Integer] -> Integer
sumaCuadradosImparesR'' [] = 0
sumaCuadradosImparesR'' (x:xs)
    | odd x = x*x + sumaCuadradosImparesR'' xs
    | otherwise = sumaCuadradosImparesR'' xs

-- 4.8.3 - Comprobar con QuichCheck que amabas definiciones son equivalentes.

prop_sumaCuadradosImpares :: [Integer] -> Bool
prop_sumaCuadradosImpares xs =
    sumaCuadradosImparesC'' xs == sumaCuadradosImparesR'' xs

-- La comrpobación es
--
-- ghci> quickCheck prop_sumaCuadradosImpares
-- +++ OK, passed 100 tests.
--
-- 4.9 - Intervalo numérico
-- -----------------------------------------------------------------------------
-- 4.9.1 - Definir, usando funciones predefinidas, la función
--
-- entreL :: Integer -> Integer -> [Integer]
--
-- tal que (entreL m n) es la lista de los números entre m y n. Por ejemplo,
-- 
-- entreL 2 5 == [2,3,4,5]

entreL :: Integer -> Integer -> [Integer]
entreL m n = [m..n]

-- 4.9.2 - Definir por recursión, la función
--
-- entreR :: Integer -> Integer -> [Integer]
--
-- tal que (entreR m n) es la lista de los número entre m y n. Por ejemplo,
-- 
-- entreR 2 5 == [2,3,4,5]

entreR :: Integer -> Integer -> [Integer]
entreR m n | m > n = []
           | otherwise = m : entreR (m+1) n

-- 4.9.3 - Comrpobar con QuickCheck que amabas definiciones son equivalentes.

prop_entre :: Integer -> Integer -> Bool
prop_entre m n =
    entreL m n == entreR m n

-- La comrpobación es
--
-- ghci> quickCheck prop_entre
-- +++ OK, passed 100 tests.
--
-- 4.10 - Mirades de los pares
-- -----------------------------------------------------------------------------
-- 4.10.1 - Definir, por comprensión, la función
--
-- mitadParesC :: [Int] -> [Int]
--
-- tal que (mitadParesC xs) es la lista de las mitades de los elementos de xs 
-- que son pares. Por ejemplo,
--
-- mitadParesC [0,2,1,7,8,56,17,18] == [0,1,4,28,9]

mitadParesC :: [Int] -> [Int]
mitadParesC xs = [x `div` 2 | x <- xs, x `mod` 2 == 0]

-- 4.10.2 - Definir, por recursión, la función
--
-- mitadParesR :: [Int] -> [Int]
--
-- tal que (mitadParesR xs) es la lista de las mitades de los elementos de xs 
-- que son pares. Por ejemplo,
--
-- mitadParesR [0,2,1,7,8,56,17,18] == [0,1,4,28,9]

mitadParesR :: [Int] -> [Int]
mitadParesR [] = []
mitadParesR (x:xs)
    | even x    = x `div` 2 : mitadParesR xs
    | otherwise = mitadParesR xs

-- 4.10.3 - Comprobar con QuickCheck que ambas definiciones son equivalentes.

prop_mitadPares :: [Int] -> Bool
prop_mitadPares xs =
    mitadParesC xs == mitadParesR xs

-- La comrpobación es
--
-- ghci> quickCheck prop_mitadPares
-- +++ OK, passed 100 tests.
--
-- 4.11 - Pertenencia a un rango
-- -----------------------------------------------------------------------------
-- 4.11.1 - Definir, por comprensión, la función
--
-- enRangoC :: Int -> Int -> [Int] -> [Int]
--
-- tal que (enRangoC a b xs) es la lista de los elementos de xs mayores o 
-- iguales que a y menores o iguales que b. Por ejemplo,
--
-- enRangoC 5 10 [1..15] == [5,6,7,8,9,10]
-- enRangoC 10 5 [1..15] == []
-- enRangoC 5 5 [1..15] == [5]

enRangoC :: Int -> Int -> [Int] -> [Int]
enRangoC a b xs = [x | x <- xs, a <= x, x <= b]

-- 4.11.2 - Definir, por recursión, la función
--
-- enRangoR :: Int -> Int -> [Int] -> [Int]
--
-- tal que (enRangoR a b xs) es la lista de los elementos de xs mayores o 
-- iguales que a y menores o iguales que b. Por ejemplo,
--
-- enRangoR 5 10 [1..15] == [5,6,7,8,9,10]
-- enRangoR 10 5 [1..15] == []
-- enRangoR 5 5 [1..15]  == [5]

enRangoR :: Int -> Int -> [Int] -> [Int]
enRangoR a b [] = []
enRangoR a b (x:xs)
    | a <= x && x <= b = x : enRangoR a b xs
    | otherwise = enRangoR a b xs

-- 4.11.3 - Comprobar con QuickCheck que ambas definiciones son equivalentes.

prop_enRango :: Int -> Int -> [Int] -> Bool
prop_enRango a b xs =
    enRangoC a b xs == enRangoR a b xs

-- La comrpobación es
--
-- ghci> quickCheck prop_enRango
-- +++ OK, passed 100 tests.
--
-- 4.12 - Suma de elementos positivos
-- -----------------------------------------------------------------------------
-- 4.12.1 - Definir, por comprensión, la función
--
-- sumaPositivosC :: [Int] -> Int
--
-- tal que (sumaPositivosC xs) es la suma de los números positivos de xs. Por 
-- ejemplo,
-- 
-- sumaPositivosC [0,1,-3,-2,8,-1,6] == 15

sumaPositivosC :: [Int] -> Int
sumaPositivosC xs = sum [x | x <- xs, x > 0]

-- 4.12.2 - Definir, por recursión, la función
--
-- sumaPositivosR :: [Int] -> Int
--
-- tal que (sumaPositivosR xs) es la suma de los números positivos de xs. Por ejemplo,
--
-- sumaPositivosR [0,1,-3,-2,8,-1,6] == 15

sumaPositivosR :: [Int] -> Int
sumaPositivosR [] = 0
sumaPositivosR (x:xs) | x > 0 = x + sumaPositivosR xs
                      | otherwise = sumaPositivosR xs

-- 4.12.3 - Comprobar con QuickCheck que ambas definiciones son equivalentes.

prop_sumaPositivos :: [Int] -> Bool
prop_sumaPositivos xs =
    sumaPositivosC xs == sumaPositivosR xs

-- La comrpobación es
--
-- ghci> quickCheck prop_sumaPositivos
-- +++ OK, passed 100 tests.
--
-- 4.13 - Aproximación del número pi
-- -----------------------------------------------------------------------------
--
-- La suma de la serie
--
--      1     1     1     1
--     --- + --- + --- + --- + ...
--     1^2   2^2   3^2   4^2
--
-- es pi²/6. Por tanto, Pi se puede aproximar mediante la raíz cuadrada de 6 
-- por la suma de la serie.
--
-- 4.13.1 - Definir, por comprensión, la función aproximaPiC tal que (
-- aproximaPiC n) es la aproximación de pPi obtenida mediante n términos de la 
-- serie. Por ejemplo,
--
-- aproximaPiC 4 == sqrt(6*(1/1^2 + 1/2^2 + 1/3^2 + 1/4^2))
--               == 2.9226129861250305
-- aproximaPiC 1000 == 3.1406380562059946

aproximaPiC n = sqrt(6*sum [1/x^2 | x <- [1..n]])

-- 4.13.2 - Definir, por comprensión, la función aproximaPiR tal que (
-- aproximaPiR n) es la aproximación de Pi obtenida mediante n términos 
-- de la serie. Por ejemplo,
--
-- aproximaPiR 4 == sqrt(6*(1/1^2 + 1/2^2 + 1/3^2 + 1/4^2))
--               == 2.9226129861250305
-- aproximaPiR 1000 == 3.1406380562059946

aproximaPiR n = sqrt(6*aproximaPiR' n)
aproximaPiR' 1 = 1
aproximaPiR' n = 1/n^2 + aproximaPiR' (n-1)

-- 4.14 - Sustitución de impares por el siguiente par
-- -----------------------------------------------------------------------------
-- 4.14.1 - Definir, por recursión, la función
--
-- sustituyeImpar :: [Int] -> [Int]
--
-- tal que (sustituyeImpar xs) es la lista obtenida sustituyendo cada número 
-- impar de xs por el siguiente número par. Por ejemplo,
--
-- sustituyeImpar [2,5,7,4] == [2,6,8,4]

sustituyeImpar :: [Int] -> [Int]
sustituyeImpar []     = []
sustituyeImpar (x:xs) | odd x     = (x+1): sustituyeImpar xs
                      | otherwise = x:sustituyeImpar xs

-- 4.14.2 - Comprobar con QuickCheck la siguient propiedad: para cualquier 
-- lista de número enteros xs, todos los elementos de la lista (sustituyeImpar
--  xs) son número pares.

prop_sustituyeImpar :: [Int] -> Bool
prop_sustituyeImpar xs = and [even x | x <- sustituyeImpar xs]

-- La comrpobación es
--
-- ghci> quickCheck prop_sustituyeImpar
-- +++ OK, passed 100 tests.
--
-- 4.15 - La compra de una persona agarrada
-- -----------------------------------------------------------------------------
-- 4.15.1 - Una persona es tan agarrada que sólo compra cuando le hacen un 
-- descuento del 10% y el precio (con el descuento) es menor o igual que 199.
-- Definir, usando comprensión, la función
--
-- agarradoC :: [Float] -> Float
--
-- talq ue (agarradoC ps) es el precio que tiene que pagar por una compra cuya 
-- lista de precios es ps. Por ejemplo,
--
-- agarradoC [45.00, 199.00, 220.00, 399.00] == 417.59998

agarradoC :: [Float] -> Float
agarradoC ps = sum [p * 0.9 | p <- ps, p * 0.9 <= 199]

-- 4.15.2 - Definir, por recursión, la función
--
-- agarradoR :: [Float] -> Float
--
-- tal que (agarradoR ps) es el precio que tiene que pagar por una compra cuya 
-- lista de precios es ps. Por ejemplo,
--
-- agarradoR [45.00, 199.00, 220.00, 399.00] == 417.59998

agarradoR :: [Float] -> Float
agarradoR [] = 0
agarradoR (p:ps)
    | precioConDescuento <= 199 = precioConDescuento + agarradoR ps
    | otherwise                 = agarradoR ps
    where precioConDescuento    = p * 0.9

-- 4.15.3 - Comprobar con QuickCheck que ambas definiciones son similares; es 
-- decir, el valor obsoluto de su diferencia es menor que una décima.

prop_agarrado :: [Float] -> Bool
prop_agarrado xs = abs (agarradoR xs - agarradoC xs) <= 0.1

-- La comrpobación es
--
-- ghci> quickCheck prop_agarrado
-- +++ OK, passed 100 tests.
--
-- 4.16 - Descomposición en productos de factores primos
-- -----------------------------------------------------------------------------
-- 4.16.1 - Lista de los factores primos de un número
-- Definir la función
--
-- factores :: Integer -> Integer
--
-- tal que (factores n) es la lista de los factores de n. Por ejemplo,
--
-- factores 60 == [1,2,3,4,5,6,10,12,15,20,30,60]

factores :: Integer -> [Integer]
factores n = [x | x <- [1..n], mod n x == 0]

-- 4.16.2 - Decidir si un número es primo
-- Definir la función
--
-- primo :: Integer -> Bool
--
-- tal que (primo n) se verifica si n es primo. Por ejemplo,
--
-- primo 7 == True
-- primo 9 == False

primo :: Integer -> Bool
primo x = factores x == [1,x]

-- 4.16.3 - Factorización de un número
-- Definir la función
--
-- factoresPrimos :: Integer -> [Integer]
--
-- tal que (factoresPrimos n) es la lista de los factores primos de n. Por ejemplo,
--
-- factoresPrimos 60 == [2,3,5]

factoresPrimos :: Integer -> [Integer]
factoresPrimos n = [x | x <- factores n, primo x]

-- 4.16.4 - Exponente de la mayor potencia de un número que divide a otro
-- 4.16.4.1 - Definir, por recursión, la función
--
-- mayorExponenteR :: Integer -> Integer -> Integer
--
-- tal que (mayorExponenteR a b) es el exponente de la mayor potencia de a que 
-- divide a b. Por ejemplo,
-- 
-- mayorExponenteR 2 8   == 3
-- mayorExponenteR 2 9   == 0
-- mayorExponenteR 5 100 == 2
-- mayorExponenteR 2 60  == 2

mayorExponenteR :: Integer -> Integer -> Integer
mayorExponenteR a b
    | mod b a /= 0 = 0
    | otherwise    = 1 + mayorExponenteR a (b `div` a)

-- 4.16.4.2 - Definir, por comprensión, la función
--
-- mayorExponenteC :: Integer -> Integer -> Integer
--
-- tal que (mayorExponenteC a b) es el exponente de la mayor potencia de a que 
-- divide a b. Por ejemplo,
-- 
-- mayorExponenteC 2 8 == 3
-- mayorExponenteC 5 100 == 2
-- mayorExponenteC 5 101 == 0

mayorExponenteC :: Integer -> Integer -> Integer
mayorExponenteC a b = head [x-1 | x <- [0..], mod b (a^x) /= 0]

-- 4.16.4.3 - Definir la función
--
-- factorizacion :: Integer -> [(Integer,Integer)]
--
-- tal que (factorización n) es la factorización de n. Por ejemplo,
--
-- factorizacion 60 == [(2,2),(3,1),(5,1)]

factorizacion :: Integer -> [(Integer,Integer)]
factorizacion n = [(x,mayorExponenteR x n) | x <- factoresPrimos n]

-- 4.16.5 - Expansión de la factorización de un número
-- 4.16.5.1 - Definir, por recursión, la función
--
-- expansionR :: [(Integer,Integer)] -> Integer
--
-- tal que (expansonR xs) es la expansión de la factorización de xs. Por ejemplo,
--
-- expansionR [(2,2),(3,1),(5,1)] == 60

expansionR :: [(Integer,Integer)] -> Integer
expansionR []         = 1
expansionR ((x,y):zs) = x^y * expansionR zs

-- 4.16.5.2 - Definir, por comprensión, la función
-- 
-- expansionC :: [(Integer,Integer)] -> Integer
--
-- tal que (expansionC xs) es la expansión de la factorización de xs. Por 
-- ejemplo,
--
-- expansionC [(2,2),(3,1),(5,1)] == 60

expansionC :: [(Integer,Integer)] -> Integer
expansionC xs = product [x^y | (x,y) <- xs]

-- 4.16.5.3 - Comprobar con QuickCheck que ambas definiciones son similares
--
-- prop_factorizacion :: Integer -> Bool
-- 
-- tal que (prop_factorizaion n) se verifica si para todo número natural x, 
-- menor o igual que n, se tiene que (expansionC (factorizacion x)) es igual a 
-- x. Por ejemplo,

prop_factorizacion n =
    and [expansionC (factorizacion x) == x | x <- [1..n]]

-- La comrpobación es
--
-- ghci> quickCheck prop_factorizacion
-- +++ OK, passed 100 tests.
--
-- 4.17 - Menor número con todos los dígitos en la factorización de su 
-- factorial
-- -----------------------------------------------------------------------------
-- El enunciado del problema 652 de "Números y algo más" es el siguiente:
--
-- Si factorizamos los factoriales de un número en función de sus divisores 
-- primos y sus potencias, ¿cuál es el menor número n tal que entre los 
-- factores primos y los exponentes de la factorización de n! están todos los
-- dígitos del cero al nueve? Por ejemplo,
--
-- * 6! = 2^4 3^2 5^1, le faltan los dígitos 0,6,7,8 y 9
-- * 12! = 2^10 3^5 5^2 7^1 11^1, le faltan los dígitos 4,6,8 y 9
--
-- Definir la función
--
-- digitosDEFactorizacion :: Integer -> [Integer]
--
-- tal que (digitosDeFactorizacion n) es el conjunto de los dígitos que 
-- aparecen en la factorización de n. Por ejemplo,
--
-- digitosDeFactorizacion (factorial 6) == [1,2,3,4,5]
-- digitosDeFactorizacion (factorial 12) == [0,1,2,3,5,7]
--
-- Usando la función anterior, calcular la solución del problema.

digitosDeFactorizacion652 :: Integer -> [Integer]
digitosDeFactorizacion652 n =
    sort (nub (concat [digitos652 x | x <- numerosDeFactorizacion652 n]))

-- donde se usan las siguientes funciones auxiliares
--
-- (digitos n) es la lista de los dígitos del número n. Por ejemplo,
--
-- digitos 320274 == [3,2,0,2,7,4]

digitos652 :: Integer -> [Integer]
digitos652 n = [read [x] | x <- show n]

-- (numerosDeFactorizacion n) es el conjunto de los números en la factorización
-- de n. Por ejemplo,
--
-- numerosDeFactorizacion 60 == [1,2,3,5]

numerosDeFactorizacion652 :: Integer -> [Integer]
numerosDeFactorizacion652 n =
    sort (nub (aux (factorizacion652 n)))
    where aux [] = []
          aux ((x,y):zs) = x : y : aux zs

-- (factorizacion n) es la factorization de n. Por ejemplo,
--
-- factorizacion 300 == [(2,2),(3,1),(5,2)]

factorizacion652 :: Integer -> [(Integer,Integer)]
factorizacion652 n =
    [(head xs, fromIntegral (length xs)) | xs <- group (factorizacion652' n)]

-- (factorizacion' n) es la lista de todos los factores primos de n; es decir, 
-- es una lista de números primos cuyo producto es n. Por ejemplo,
--
-- factorizacion 300 == [2,2,3,5,5]

factorizacion652' :: Integer -> [Integer]
factorizacion652' n | n == 1 = []
                 | otherwise = x : factorizacion652' (div n x)
                 where x = menorFactor652 n

-- (menorFactor n) es el menor factor primo de n. Por ejemplo,
--
-- menorFactor 15 == 3

menorFactor652 :: Integer -> Integer
menorFactor652 n = head [x | x <- [2..], rem n x == 0]

-- (factorial n) es el factorial de n. Por ejemplo,
--
-- factorial 5 == 120

factorial652 :: Integer -> Integer
factorial652 n = product [1..n]

-- Para calcular la solución, se define la constante

solucion =
    head [n | n <- [1..], digitosDeFactorizacion652 (factorial652 n) == [0..9]]

-- El cálculo de la solución es
--
-- ghci> solucion
-- 49
--
-- 4.18 - Suma de números especiales
-- -----------------------------------------------------------------------------
-- Los siguientes ejercicios están basados en el problema 357 del proyecto Euler
-- 4.18.1 - Un número natual n es especial si para todo divisor d de n, d + n/d es primo.
-- Definir la función
--
-- especial :: Integer -> Bool
--
-- tal que (especial x) se verifica si x es especial. Por ejemplo,
-- especial 30 == True
-- especial 20 == False

especial357 :: Integer -> Bool
especial357 x = and [esPrimo357 (d + x `div` d) | d <- divisores357 x]

-- donde se usan las siguientes funciones auxiliares
--
-- (divisores x) es la lista de los divisores de x. Por ejemplo,
--
-- divisores 30 == [1,2,3,5,6,10,15,30]

divisores357 :: Integer -> [Integer]
divisores357 x = [d | d <- [1..x], x `rem` d == 0]

-- (esPrimo x) se verifica si x es primo. Por ejemplo,
--
-- esPrimo 7 == True
-- esPrimo 8 == False

esPrimo357 :: Integer -> Bool
esPrimo357 x = divisores357 x == [1,x]

-- 4.18.2 - Definir, por comprensión, la función
--
-- sumaEspeciales :: Integer -> Integer
--
-- tal que (sumaEspeciales n) es la suma de los números especiales menores o 
-- iguales que n. Por ejemplo,
-- 
-- sumaEspeciales 100 == 401

sumaEspeciales :: Integer -> Integer
sumaEspeciales n = sum [x | x <- [1..n], especial357 x]

-- 4.18.3 - Defirni, por recursión, la función
--
-- sumaEspecialesR :: Integer -> Integer
--
-- tal que, (sumaEspecialesR n) es la suma de los números especiales menores o 
-- iguales que n. Por ejemplo,
--
-- sumaEspecialesR 100 == 401

sumaEspecialesR :: Integer -> Integer
sumaEspecialesR 0 = 0
sumaEspecialesR n | especial357 n = n + sumaEspecialesR (n-1)
                  | otherwise  = sumaEspecialesR (n-1)

-- 4.19 - Distancia de Hamming
-- -----------------------------------------------------------------------------
-- La distancia de Hamming entre dos listas es el número de posiciones en que
-- los correspondientes elementos son distintos. Por ejemplo, la distancia de
-- Hamming entre "roma" y "loba" es 2 (porque hay 2 posiciones en las que los
-- elementos correspondientes son distintos: la 1ª y la 3ª).
-- 4.19.1 - Definir, por comprensión, la función
--
-- distanciaC :: Eq a => [a] -> [a] -> Int
--
-- tal que (distanciaC xs ys) es la distanciaC de Hamming entre xs e ys. Por 
-- ejemplo,
--
-- distanciaC "romano" "comino" == 2
-- distanciaC "romano" "camino" == 3
-- distanciaC "roma" "comino" == 2
-- distanciaC "roma" "camino" == 3
-- distanciaC "romano" "ron" == 1
-- distanciaC "romano" "cama" == 2
-- distanciaC "romano" "rama" == 1

distanciaC :: Eq a => [a] -> [a] -> Int
distanciaC xs ys = length [(x,y) | (x,y) <- zip xs ys, x /= y]

-- 4.19.2 - Definir, por recursión, la función
--
-- distanciaR :: Eq a => [a] -> [a] -> Int
--
-- tal que (distanciaR xs ys) es la distanciaR de Hamming entre xs e ys. Por 
-- ejemplo,
--
-- distanciaR "romano" "comino" == 2
-- distanciaR "romano" "camino" == 3
-- distanciaR "roma" "comino" == 2
-- distanciaR "roma" "camino" == 3
-- distanciaR "romano" "ron" == 1
-- distanciaR "romano" "cama" == 2
-- distanciaR "romano" "rama" == 1

distanciaR :: Eq a => [a] -> [a] -> Int
distanciaR [] ys = 0
distanciaR xs [] = 0
distanciaR (x:xs) (y:ys) | x /= y = 1 + distanciaR xs ys
                         | otherwise = distanciaR xs ys

-- 4.19.3 - Comprobar con QuickCheck que ambas definiciones son equivalentes.

prop_distancia :: [Int] -> [Int] -> Bool
prop_distancia xs ys =
    distanciaC xs ys == distanciaR xs ys

-- La comrpobación es
--
-- ghci> quickCheck prop_distancia
-- +++ OK, passed 100 tests.
--
-- 4.20 - Traspuesta de una matriz
-- -----------------------------------------------------------------------------
-- Definir la función
-- 
-- traspuesta :: [[a]] -> [[a]]
--
-- tal que (traspuesta m) es la traspuesta de la matriz m. Por ejemplo,
--
-- traspuesta [[1,2,3],[4,5,6]]   == [[1,4],[2,5],[3,6]]
-- traspuesta [[1,4],[2,5],[3,6]] == [[1,2,3],[4,5,6]]

traspuesta :: [[a]] -> [[a]]
traspuesta []           = []
traspuesta ([]:xss)     = traspuesta xss
traspuesta ((x:xs):xss) =
    (x:[h | (h:_) <- xss]) : traspuesta (xs : [t | (_:t) <- xss])

-- 4.21 - Números expresables como sumas acotadas de elementos de una lista
-- -----------------------------------------------------------------------------
-- Definir la función
--
-- sumas :: Int -> [Int] -> [Int]
--
-- tal que (sumas n xs) es la lista de los números que se pueden obtener como 
-- suma de n, o menos, elementos de xs. Por ejemplo,
--
-- sumas 0 [2,5]   == [0]
-- sumas 1 [2,5]   == [2,5,0]
-- sumas 2 [2,5]   == [4,7,2,10,5,0]
-- sumas 3 [2,5]   == [6,9,4,12,7,2,15,10,5,0]
-- sumas 2 [2,3,5] == [4,5,7,2,6,8,3,10,5,0]

sumas :: Int -> [Int] -> [Int]
sumas 0 _      = [0]
sumas _ []     = [0]
sumas n (x:xs) = [x+y | y <- sumas (n-1) (x:xs)] ++ sumas n xs