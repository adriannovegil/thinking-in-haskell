module Recursion where

import Data.List
import Test.QuickCheck

main :: IO ()
main = return ()

-- 4.1 - Suma de los cuadrados de los primeros números
-- -----------------------------------------------------------------------------
-- 4.1.1 - Suma de los cuadrados de los primeros números (recursión)
-- Definir, por recursión, la función
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
-- 4.1.3 - Suma de los cuadrados de los primeros números (comprensión)
-- Definir, por comprensión, la función
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
-- 4.2.1 - Número de bloques de escaleras triangulares (recursión)
-- definir, por recursión, la función
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

-- 4.2.2 - Número de bloques de escaleras triangulares (comprensión)
-- Definir, por comprensión, la función
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

