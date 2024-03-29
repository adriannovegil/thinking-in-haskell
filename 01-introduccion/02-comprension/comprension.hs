module Comprension where

main :: IO ()
main = return ()

-- 2.1 - Suma de los cuadrados de los n primeros números.
-- -----------------------------------------------------------------------------
-- Definir, por comprensión, la función
--
-- sumaDeCuadrados :: Integer -> Integer
--
-- tal que (sumaDeCuadrados n) es la suma de los cuadrados de los primeros n
-- números; es decir, 1^2 + 2^2 + ... + n^2. Por ejemplo,
--
-- sumaDeCuadrados 3 = 14
-- sumaDeCuadrados 100 = 338350

sumaDeCuadrados :: Integer -> Integer
sumaDeCuadrados n = sum [x^2 | x <- [1..n]]

-- 2.2 - Listas con un elemento replicado.
-- -----------------------------------------------------------------------------
-- Definir por comprensión la función
--
-- replica :: Int -> a -> [a]
--
-- tal que (replica n x) es la lista formada por n copias del elemento x. Por
-- ejemplo,
--
-- replica 3 True == [True, True, True]

replica :: Int -> a -> [a]
replica n x = [x | _ <- [1..n]]

-- 2.3 - Triángulos aritméticos.
-- -----------------------------------------------------------------------------
--
-- 2.3.1 - Definir la función suma tal que (suma n) es la suma de los n primeros
-- números. Por ejemplo,
--
-- suma 3 == 6

suma n = sum [1..n]
suma' n = (1 + n) * n `div` 2

-- 2.3.2 - Los triángulos aritméticos se forman somo sigue.
--
--  1
--  2  3
--  4  5  6
--  7  8  9 10
-- 11 12 13 14 15
-- 16 16 18 19 20 21
--
-- Definir la función línea tal que (linea n) es la línea n-ésima de los
-- triángulos aritméticos. Por ejemplo,
--
-- linea 4 == [7,8,9,10]
-- linea 5 == [11,12,13,14,15]

linea n = [suma (n - 1) + 1 .. suma(n)]

-- 2.3.3 - definir la función triángulo tal que (triangulo n) es el
-- triángulo artimético de altura n. Por ejemplo,
--
-- triangulo 3 = [[1], [2,3], [4,5,6]]
-- triangulo 4 = [[1], [2,3], [4,5,6], [7,8,9,10]]

triangulo n = [linea m | m <- [1..n]]

-- 2.4 - Números perfectos.
-- -----------------------------------------------------------------------------
-- Un enetero positivo es perfecto si es igual a la suma de sus factores,
-- excluyendo el propio número. Definir por comprensión la función
--
-- perfectos :: Int -> [Int]
--
-- tal que (perfectos n) es la lista de todos los números perfectos menores
-- que n. Por ejemplo,
--
-- perfectos 500 == [6, 28, 496]

factores :: Int -> [Int]
factores n = [x | x <- [1..n], n `mod` x == 0]

perfectos :: Int -> [Int]
perfectos n = [x | x <- [1..n], sum (init (factores x)) == x]

-- 2.5 - Números abundantes.
-- -----------------------------------------------------------------------------
-- Un número natural n se denomina abundante si es menor que la suma de sus
-- divisores propios. Por ejemplo, 12 y 30 son abundantes pero 5 y 28 no lo son.
--
-- 2.5.1 - Definir una función numeroAbundante tal que (numeroAbundante n) se
-- verifica si n es un número abundante. Por ejemplo,
--
-- numeroAbundante 5 == False
-- numeroAbundante 12 == True
-- numeroAbundante 28 == False
-- numeroAbundante 30 == True

divisores :: Int -> [Int]
divisores n = [m | m <- [1..n-1], n `mod` m == 0]

numeroAbundante :: Int -> Bool
numeroAbundante n = n < sum (divisores n)

-- 2.5.2 - Definir una función numerosAbundantesMenores tal que
-- (numerosAbundantesMenores n) es la lista de número abundantes menores o
-- iguales que n. Por ejemplo,
--
-- numerosAbundantesMenores 50 = [12,18,20,24,30,36,40,42,48]

numerosAbundantesMenores :: Int -> [Int]
numerosAbundantesMenores n = [x | x <- [1..n], numeroAbundante x]

-- 2.5.3 - Definir la función todosPares tal que (todosPares n) se verifica si
-- todos los números abundantes menores o iguales que n son pares. Por
-- ejemplo,
--
-- todosPares 10 = True
-- todosPares 100 = True
-- todosPares 1000 = False

todosPares :: Int -> Bool
todosPares n = and [even x | x <- numerosAbundantesMenores n]

-- 2.5.4 - Definir la constante primerAbundanteImpar que calcule el primer número
-- natural abundante impar. Determinar el valor de dicho número.

primerAbundanteImpar :: Int
primerAbundanteImpar = head [x | x <- [1..], numeroAbundante x, odd x]

-- *Main> primerAbundanteImpar
-- 945
--
-- 2.6 - Problema 1 del Proyecto Euler.
-- -----------------------------------------------------------------------------
-- Definir la función
--
-- euler1 :: Integer -> Integer
--
-- tal que (euler1 n) es la suma de todos los múltiplos de 3 ó 5 menores que n.
-- Por ejemplo,
--
-- euler1 10 == 23

euler1 :: Integer -> Integer
euler1 n = sum [x | x <- [1..n-1], multiplo x 3 || multiplo x 5]
    where multiplo x y = mod x y == 0

-- *Main> euler1 1000
-- 233168
--
-- 2.7 - Número de pares de naturales en un círculo.
-- -----------------------------------------------------------------------------
-- Definir la función
--
-- circulo :: Integer -> Integer
--
-- tal que (circulo n) es la cantidad de pares de números naturales (x, y) que
-- se encuentran dentro del círculo de rario n. Por ejemplo,
--
-- circulo 3 = 9
-- circulo 4 = 15
-- circulo 5 = 22

circulo :: Int -> Int
circulo n = length [(x,y) | x <- [0..n], y <- [0..n], x^2 + y^2 < n^2]

-- 2.8 - Aproximación del número e.
-- -----------------------------------------------------------------------------
--
-- 2.8.1 - Definir la función aproxE tal que (aproxE n) es la lista cuyos
-- elementos son los términos de la sucesión (1 + 1/m )^m desde 1 hasta n. Por
-- ejemplo,
--
-- aproxE 1 == [2.0]
-- aproxE 4 == [2.0, 2.25, 2.37037037037037, 2.44140625]

aproxE n = [(1 + 1 / m) ** m | m <- [1..n]]

-- 2.8.2 - Cual es el límite de la sucesión (1 + 1/m )^m?
--
-- Solución: El límite de la sucesión, cuando n tiende a infinito, es el
-- número e.
--
-- 2.8.3 - Definir la función errorR tal que (errorAproxE x) es el menor número
-- de términos de la sucesión (1 + 1/m )^m necesarios para obtener su límite
-- con un error menor que x.
-- Por ejemplo,
--
-- errorAproxE 0.1 == 13.0
-- errorAproxE 0.01 == 135.0
-- errorAproxE 0.001 == 1359.0
--
-- Indicación: En Erlang, e se calcula como math:exp(1).

errorAproxE x = head [m | m <- [1..], abs((exp 1) - (1 + 1 / m) ** m) < x]

-- 2.8.4 - El número e también se puede definir como la suma de la siere:
--
-- 1/0! + 1/1! + 1/2! + 1/3! + ...
--
-- Definir la función aproxE2 tal que (aproxE2 n) es la aproximación de e que
-- se obtiene sumando los términos de la serie hasta 1/n!. Por ejemplo,
--
-- aproxE' 10 = 2.718281801146385
-- aproxE' 100 = 2.7182818284590455

aproxE' n = 1 + sum [1 / factorial k | k <- [1..n]]

factorial n = product [1..n]

-- 2.8.5 - Definir la constante e como 2,71828459.

e = 2.71828459

-- 2.8.6 - Definir la función errorE' tal que (errorAproxE2 x) es el menor
-- número de términos de la serie anterior necesarios para obtener e con un
-- error menor que x. Por ejemplo,
--
-- errorE' 0.1 == 3.0
-- errorE' 0.01 == 4.0
-- errorE' 0.001 == 6.0
-- errorE' 0.0001 == 7.0

errorE' x = head [n | n <- [0..], abs(aproxE' n - e) < x]

-- 2.9 - Aproximación del límite.
-- -----------------------------------------------------------------------------
--
-- 2.9.1 - Definir la función aproxLimSeno tal que (aproxLimSeno n) es la lista
-- cuyos elementos son los términos de la sucesión sen(1/m)/(1/m) desde 1 hasta
-- n. Por ejemplo,
--
-- aproxLimSeno 1 = [0.8114709848078965]
-- aproxLimSeno 2 = [0.8114709848078965, 0.958851077208406]

aproxLimSeno n = [sin(1/m) / (1/m) | m <- [1..n]]

-- 2.9.2 - ¿Cuál es el límite de la sucesión sen(1/m)/(1/m)?
--
-- Solución: El límite es 1.
--
-- 2.9.3 - Definir la función errorLimSeno tal que (errorLimSeno x) es el menor
-- número de términos de la sucecsión sen(1/m)/(1/m) necesarios para obtener
-- su límite con un error menor que x. Por ejemplo,
--
-- errorLimSeno 0.1 == 2.0
-- errorLimSeno 0.01 == 5.0
-- errorLimSeno 0.001 == 13.0
-- errorLimSeno 0.0001 == 41.0

errorLimSeno x = head [m | m <- [1..], abs(1 - sin(1/m) / (1/m)) < x]

-- 2.10 - Cálculo del número pi.
-- -----------------------------------------------------------------------------
--
-- 2.10.1 - definir la función calculaPi tal que (calculaPi n) es la aproximación
-- del número pi calculada mediante la expresión
--
-- 4 * (1 - 1/3 + 1/5 - 1/7 + ... + (-1)^n/(2n+1))
--
-- Por ejemplo,
--
-- calculaPi 3 == 2.8952380952380956
-- calculaPi 300 == 3.1449149035588526

calculaPi n = 4 * sum [(-1)**x / (2 * x + 1) | x <- [0..n]]

-- 2.10.2 - Definir la función errorPi tal que (errorPi x) es el menor número de
-- términos de la serie
--
-- 4 * (1 - 1/3 + 1/5 - 1/7 + ... + (-1)^n/(2n+1))
--
-- necesarios para obtener pi con un error menor que x. Por ejemplo,
--
-- errorPi 0.1 == 9.0
-- errorPi 0.01 == 99.0
-- errorPi 0.001 == 999.0

errorPi x = head [ n | n <- [1..], abs (pi - (calculaPi n)) < x]

-- 2.11 - Ternas pitagóricas.
-- -----------------------------------------------------------------------------
--
-- 2.11.1 - Una terna (x,y,z) de enteros positivos es pitagórica si x^2+Y^2 = z^2.
-- Usando una lista por comprensión, definir la función
--
-- pitagoricas :: Int -> [(Int,Int,Int)]
--
-- tal que (pitagoricas n) es la lista de todas las ternas pitagóricas cuyas
-- componentes están entre 1 y n. Por ejemplo,
--
-- pitagoricas 10 == [(3,4,5), (4,3,5), (6,8,10), (8,6,10)]

pitagoricas :: Int -> [(Int,Int,Int)]
pitagoricas n = [(x,y,z) | x <- [1..n],
                           y <- [1..n],
                           z <- [1..n],
                           x^2 + y^2 == z^2]

-- 2.11.2 - Definir la función
--
-- numeroDePares :: (Int, Int, Int) ->
--
-- tal que (numeroDePares t) es el número de elementos pares de la terna t. Por
-- ejemplo,
--
-- numeroDePares (3,5,7) == 0
-- numeroDePares (3,6,7) == 1
-- numeroDePares (3,6,4) == 2
-- numeroDePares (4,6,4) == 3

numeroDePares :: (Int,Int,Int) -> Int
numeroDePares (x,y,z) = sum [1 | n <- [x,y,z], even n]

-- 2.11.3 - Definir la función
--
-- conjetura :: int -> Bool
--
-- tal que (conjetura n) se verifica si todas las ternas pitagóricas cuyas
-- componentes están entre 1 y n tienen un número impar de números pares. Por
-- ejemplo,
--
-- conjetura 10 == true

conjetura :: Int -> Bool
conjetura n = and [odd (numeroDePares t) | t <- pitagoricas n]

-- 2.11.4 Demostrar la conjetura para todas las ternas pitagóricas.
--
-- Solución: Sea (x,y,z) una terna pitagórica. Entonces x^2+Y^2 = z^2. Pueden
-- darse 4 casos:
-- Caso 1: x e y son pares. Entonces x^2, y^2 y z^2 también lo son. Luego el
-- número de componentes pares es 3 que es impar.
-- Caso 2: x es par e y es impar. Entonces, x^2 es par, y^2 es impar y z^2 es
-- impar. Luedo el número de componentes pares es 1 que es impar.
-- Caso 3: x es impar e y es par. Análogo al caso 2.
-- Caso 4: x e y son impares. Entonces x^2 e y^2 también son impares y z^2 es
-- par. Luego el número de componentes pares es 1 impar.
--
-- 2.12 - Problema 9 del Proyecto Euler.
-- -----------------------------------------------------------------------------
--
-- 2.12.1 - Una terna pitagórica es una terna de números naturales (a,b,c) tal que
-- a < b < c y a^2 + b^2 = c^2. Por ejemplo (3,4,5) es una terna pitagórica.
-- Definir la función
--
-- ternasPitagoricas :: int -> [[int]]
--
-- tal que (ternasPitagoricas x) es la lista de las ternas pitagóricas cuya
-- suma es x. Por ejemplo,
--
-- ternasPitagoricas 12 = [(3,4,5)]
-- ternasPitagoricas 60 = [(10,24,26), (15,20,25)]

ternasPitagoricas :: Integer -> [(Integer,Integer,Integer)]
ternasPitagoricas x = [(a,b,c) | a <- [1..x],
                                 b <- [a+1..x],
                                 c <- [x-a-b],
                                 a^2 + b^2 == c^2]

-- 2.12.2 - Definir la constante euler9 tal que euler9 es producto a*b*c donde
-- (a,b,c) es la única terna pitagórica tal que a + b + c = 1000. Calcular el
-- valor euler9.

euler9 = a*b*c
    where (a,b,c) = head (ternasPitagoricas 1000)

-- 2.13 - Producto escalar
-- -----------------------------------------------------------------------------
-- El producto escalar de dos listas de enteros xs e ys de longitud n viene dado
-- por la suma de los productos de los elementos correspondientes. Definir por
-- comprensión la función
--
-- productoEscalar :: [int] -> [int] -> int
--
-- tal que (productoEscalar xs ys) es el producto escalar de las listas xs e ys.
-- Por ejemplo,
--
-- productoEscalar [1,2,3] [4,5,6] == 32

productoEscalar :: [Int] -> [Int] -> Int
productoEscalar xs ys = sum [x*y | (x,y) <- zip xs ys]

-- 2.14 - Suma de pares de elementos consecutivos.
-- -----------------------------------------------------------------------------
-- Definir por comprensión, la función
--
-- sumaConsecutivos :: [int] -> [int]
--
-- tal que (sumaConsecutivos xs) es la suma de los pares de elementos
-- consecutivos de la lista xs. Por ejemplo,
--
-- sumaConsecutivos [3,1,5,2] == [4,6,7]
-- sumaConsecutivos [3] == []

sumaConsecutivos :: [Int] -> [Int]
sumaConsecutivos xs = [x+y | (x,y) <- zip xs (tail xs)]

-- 2.15 - Posiciones de un elemento en una lista.
-- -----------------------------------------------------------------------------
-- En el tema se ha definido la función
--
-- posiciones :: Eq a -> a -> [a] -> [int]
--
-- tal que (posiciones x xs) es la lista de las posiciones ocupadas por el
-- elemento x en la lista. Por ejemplo,
--
-- posiciones 5 [1,5,3,5,5,7] == [1,3,4]
--
-- Definir, usando la función busca (definida más abajo), la función
--
-- posiciones2 :: a -> a -> [a] -> [int]
--
-- tal que posiciones 2 sea equivalente a posiciones
-- La definición de posiciones es

posiciones :: Eq a => a -> [a] -> [Int]
posiciones x xs =
    [i | (x',i) <- zip xs [0..n], x == x']
    where n = length xs - 1

-- La definición de busca es

busca :: Eq a => a -> [(a, b)] -> [b]
busca c t = [v | (c', v) <- t, c' == c]

-- La redefinición de posiciones es

posiciones' :: Eq a => a -> [a] -> [Int]
posiciones' x xs = busca x (zip xs [0..])

-- 2.16 - Representación densa de un polinomio representado dispersamente.
-- -----------------------------------------------------------------------------
-- Los polinomios pueden representarse de forma dispersa o densa. Por ejemplo,
-- el polinomio 6x^4 - 5x^2 + 4x - 7 se puede representar de forma dispersa por
-- [6,0,-5,4,-7] y de forma densa por [(4,6),(2,-5),(1,4),(0,-7)]. Definir la
-- función
--
-- densa :: [int] -> [(int, int)]
--
-- tal que (densa xs) es la representación densa del polinomio cuya
-- representación dispersa es xs. Por ejemplo,
--
-- densa [6,0,-5,4,-7] == [(4,6),(2,-5),(1,4),(0,-7)]
-- densa [6,0,0,3,0,4] == [(5,6),(2,3),(0,4)]

densa :: [Int] -> [(Int,Int)]
densa xs = [(x,y) | (x,y) <- zip [n-1,n-2..0] xs, y /= 0]
    where n = length xs

-- 2.17 - Producto cartesiano.
-- -----------------------------------------------------------------------------
-- La función
--
-- pares :: [a] -> [b] -> [(a,b)]
--
-- definida por
--
-- pares xs ys = [(x,y) | x <- xs, y <- ys]
--
-- toma como argumento dos listas y devuelve las listas de los pares con el
-- primer elemento de la primera lista y el segundo de la segunda. Por ejemplo,
--
-- pares [1..3] [4..6]
-- [(1,4),(1,5),(1,6),(2,4),(2,5),(2,6),(3,4),(3,5),(3,6)]
--
-- Definir, usando dos listas por comprensión con un generador cada una, la
-- función
-- pares2 :: [a] -> [b] -> [(a,b)]
-- tal que pares2 sea equivalente a pares.
--
-- Indicación: Utilizar la función concat y encajar una lista por compresión
-- dentro de la otra.

pares' :: [a] -> [b] -> [(a,b)]
pares' xs ys = concat [[(x,y) | y <- ys] | x <- xs]

-- 2.18 - Consulta de bases de datos.
-- -----------------------------------------------------------------------------
-- Las bases de datos sobre actividades de personas pueden representarse
-- mediante listas de elementos de la forma (a,b,c,d), donde a es el nombre de
-- la persona, b su actividad, c su fecha de nacimiento y d la de su
-- fallecimiento. Un ejemplo es la siguiente que usaremos a lo largo de los
-- siguientes ejercicios
--
-- personas :: [(Stirng, String, Int, Int)]
-- personas = [("Cervantes", "Literatura", 1547, 1616),
--			   ("Velazquez", "Pintura", 1599, 1660),
--			   ("Picasso", "Pintura", 1881, 1973),
--			   ("Beethoven", "Musica", 1770, 1823),
--			   ("Poincare", "Ciencia", 1854, 1912),
--			   ("Quevedo", "Literatura", 1580, 1654),
--			   ("Goya", "Pintura", 1746, 1828),
--			   ("Einstein", "Ciencia", 1879, 1955),
--			   ("Mozart", "Musica", 1756, 1791),
--			   ("Botticelli", "Pintura", 1445, 1510),
--			   ("Borromini", "Arquitectura", 1599, 1667),
--			   ("Bach", "musica", 1685, 1750)]
--
-- 2.18.1 - Definir la función nombres tal que (nombres bd) es la lista de los
-- nombres de las personas de la base de datos bd. Por ejemplo,
--
-- nombres personas == ["Cervantes", "Velazquez", "Picasso", "Beethoven",
-- "Poincare", "Quevedo", "Goya", "Einstein", "Mozart", "Botticelli",
-- "Borromini", "Bach"]

nombres :: [(String,String,Int,Int)] -> [String]
nombres bd = [x | (x,_,_,_) <- bd]

-- 2.18.2 - Definir la función musicos tal que (musicos bd) es la lista de los
-- nombres de los músicos de la base de datos bd. Por ejemplo,
--
-- musicos personas == ["Beethoven", "Mozart", "Bach"]

musicos :: [(String,String,Int,Int)] -> [String]
musicos bd = [x | (x,m,_,_) <- bd, m == "Musica"]

-- 2.18.3 - Definir la función selección tal que (seleccion bd m) es la lista de
-- los nombres de las personas de la base de datos bd cuya actividad es m. Por
-- ejemplo,
--
-- selección personas "Pintura" == ["Velazquez", "Picasso", "Goya",
-- 								    "Botticelli"]

seleccion :: [(String,String,Int,Int)] -> String -> [String]
seleccion bd m = [ x | (x,m',_,_) <- bd, m == m' ]

-- 2.18.4 - Definir, usando el apartado anterior, la función musicos2 tal que
-- (musicos2 bd) es la lista de los nombres de los músicos de la base de datos
-- bd. Por ejemplo,
--
-- musicos2 personas == ["Beethoven", "Mozart", "Bach"]

musicos' :: [(String,String,Int,Int)] -> [String]
musicos' bd = seleccion bd "Musica"

-- 2.18.5 - Definir la función vivas tal que (vivas bd a) es la lista de los
-- nombres de las personas de la base de datos bd que estaban vivas en el año
-- a. Por ejemplo,
--
-- vivas personas 1600 == ["Cervantes", "Velazquez", "Quevedo", "Borromini"]

vivas :: [(String,String,Int,Int)] -> Int -> [String]
vivas ps a = [x | (x,_,a1,a2) <- ps, a1 <= a, a <= a2]