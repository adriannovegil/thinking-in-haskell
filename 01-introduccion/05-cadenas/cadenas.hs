module Cadenas where

import Data.Char
import Test.QuickCheck
import Data.List (isPrefixOf, find)

main :: IO ()
main = return ()

-- 5.1 - Suma de los dígitos de una cadena
-- -----------------------------------------------------------------------------
-- 5.1.1 - Definir, por comprensión, la función
--
-- sumaDigitosC :: String -> Int
--
-- tal que (sumaDigitosC xs) es la suma de los dígitos de la cadena xs. Por 
-- ejemplo,
--
-- sumaDigitosC "SE 2431 X" == 10
--
-- Nota: Usar las funciones isDigit y digitToInt

sumaDigitosC :: String -> Int
sumaDigitosC xs = sum [digitToInt x | x <- xs, isDigit x]

-- 5.1.2 - Definir, por recursión, la función
--
-- sumaDigitosR :: String -> Int
--
-- tal que (sumaDigitosR xs) es la suma de los dígitos de la cena xs. Por 
-- ejemplo,
--
-- sumaDigitosR "SE 2431 X" == 10
--
-- Nota: Usar las funciones isDigit y digitToInt.

sumaDigitosR :: String -> Int
sumaDigitosR [] = 0
sumaDigitosR (x:xs)
    | isDigit x = digitToInt x + sumaDigitosR xs
    | otherwise = sumaDigitosR xs

-- 5.1.3 - Comprobar con QuickCheck que ambas definiciones son equivalentes

prop_sumaDigitos :: String -> Bool
prop_sumaDigitos xs =
    sumaDigitosC xs == sumaDigitosR xs

-- La comprobación es
-- 
-- ghci> quickCheck prop_sumaDigitos
-- +++ OK, passed 100 tests.
--
-- 5.2 - Capitalización de una cadena
-- -----------------------------------------------------------------------------
-- 5.2.1 - Definir, por comprensión, la función
--
-- mayusculaInicial :: String -> String
--
-- tal que (mayusculaInicial xs) es la palabra xs con la letra inicial en 
-- mayúscula y las restantes en minúsculas. Por ejemplo,
--
-- mayusculaInicial "sEviLLa" == "Sevilla"
--
-- Nota: Usar las funciones toLowery y toUpper.

mayusculaInicial :: String -> String
mayusculaInicial []     = []
mayusculaInicial (x:xs) = toUpper x : [toLower x | x <- xs]

-- 5.2.2 - Definir, por recursión, la función
--
-- mayusculaInicialR :: String -> String
--
-- tal que (mayusculaInicialR xs) es la palabra xs con la letra inicial en 
-- mayúscula y las restantes en minúscula. Por ejemplo,
--
-- mayusculaInicialR "sEviLLa" == "Sevilla"

mayusculaInicialR :: String -> String
mayusculaInicialR [] = []
mayusculaInicialR (x:xs) = toUpper x : aux xs
    where aux (x:xs) = toLower x : aux xs
          aux []     = []

-- 5.2.3 - Comprobar con QuickCheck que ambas definiciones son equivalentes.

prop_mayusculaInicial :: String -> Bool
prop_mayusculaInicial xs =
    mayusculaInicial xs == mayusculaInicialR xs

-- La comprobación es
-- 
-- ghci> quickCheck prop_mayusculaInicial
-- +++ OK, passed 100 tests.
--
-- 5.3 - Título con las reglas de myúsculas iniciales
-- -----------------------------------------------------------------------------
-- Se consideran las siguientes reglas de mayúsculas iniciales para los títulos:
--
-- * la primera palabra comienza en mayúscula y
-- * todas las palabras que tienen 4 letras como mínimo empiezan con mayúsculas.
--
-- Definir, por comprensión, la función
--
-- titulo :: [String] -> [String]
--
-- tal que (titulo ps) es la lista de las palabras de ps con las reglas de 
-- mayúsculas iniciales de los títulos. Por ejemplo,
--
-- ghci> titulo ["eL","arTE","DE","La","proGraMacion"]
-- ["El","Arte","de","la","Programacion"]

titulo :: [String] -> [String]
titulo []     = []
titulo (p:ps) = mayusculaInicial p : [transforma p | p <- ps]

-- donde (transforma p) es la palabra p con mayúscula inicial si su longitud es 
-- mayor o igual que 4 y es p en minúscula en caso contrario.

transforma :: String -> String
transforma p | length p >= 4 = mayusculaInicial p
             | otherwise = minuscula p

-- y (minuscula xs) es la palabra xs en minúscula.

minuscula :: String -> String
minuscula xs = [toLower x | x <- xs]

-- 5.3.2 - Definir, por recursión, la función
--
-- tituloR :: [String] -> [String]
--
-- tal que (tituloR ps) es la lista de las palabras de ps con las reglas de 
-- mayúsculas iniciales de los títulos. Por ejemplo,
-- 
-- ghci> tituloR ["eL","arTE","DE","La","proGraMacion"]
-- ["El","Arte","de","la","Programacion"]

tituloR :: [String] -> [String]
tituloR [] = []
tituloR (p:ps) = mayusculaInicial p : tituloRAux ps
    where tituloRAux [] = []
          tituloRAux (p:ps) = transforma p : tituloRAux ps

-- 5.3.3 - Comprobar con QuickCheck que ambas definiciones son equivalentes.

prop_titulo :: [String] -> Bool
prop_titulo xs = titulo xs == tituloR xs

-- La comprobación es
-- 
-- ghci> quickCheck prop_titulo
-- +++ OK, passed 100 tests.
--
-- 5.4 - Búsqueda en crucigramas
-- -----------------------------------------------------------------------------
-- 5.4.1 - Definir, por comprensión, la función
-- 
-- buscaCrucigrama :: Char -> Int -> Int -> [String] -> [String]
--
-- tal que (buscaCrucigrama l pos lon ps) es la lista de las palabras de la 
-- lista de palabras ps que tienen longitud lon y poseen la letra l en la 
-- posición pos (comenzando en 0). Por ejemplo,
--
-- ghci> buscaCrucigrama 'c' 1 7 ["ocaso", "acabado", "ocupado"]
-- ["acabado","ocupado"]

buscaCrucigrama :: Char -> Int -> Int -> [String] -> [String]
buscaCrucigrama l pos lon ps =
    [p | p <- ps,
         length p == lon,
         0 <= pos, pos < length p,
         p !! pos == l]

-- 5.4.2 - Definir, por recursión, la función
--
-- buscaCrucigramaR :: Char -> Int -> Int -> [String] -> [String]
--
-- tal que (buscaCrucigramaR l pos lon ps) es la lista de las palabras de la 
-- lista de palabras ps que tienen longitud lon y posen la letra l en la 
-- posición pos (comenzando en 0). Por ejemplo,
--
-- ghci> buscaCrucigramaR 'c' 1 7 ["ocaso", "acabado", "ocupado"]
-- ["acabado","ocupado"]

buscaCrucigramaR :: Char -> Int -> Int -> [String] -> [String]
buscaCrucigramaR letra pos lon [] = []
buscaCrucigramaR letra pos lon (p:ps)
    | length p == lon && 0 <= pos && pos < length p && p !! pos == letra
        = p : buscaCrucigramaR letra pos lon ps
    | otherwise
        = buscaCrucigramaR letra pos lon ps

-- 5.4.3 - Comprobar con QuickCheck que ambas definiciones son equivalentes.

prop_buscaCrucigrama :: Char -> Int -> Int -> [String] -> Bool
prop_buscaCrucigrama letra pos lon ps =
    buscaCrucigrama letra pos lon ps == buscaCrucigramaR letra pos lon ps

-- La comprobación es
-- 
-- ghci> quickCheck prop_buscaCrucigrama
-- +++ OK, passed 100 tests.
--
-- 5.5 - Posiciones de un carácter en una cadena
-- -----------------------------------------------------------------------------
-- 5.5.1 - Definir, por comprensión, la función
--
-- posiciones :: String -> Char -> [Int]
--
-- tal que (posiciones xs y) es la lista de las posiciones del carácter y en la 
-- cadena xs. Por ejemplo,
--
-- posiciones "Salamamca" 'a' == [1,3,5,8]

posiciones :: String -> Char -> [Int]
posiciones xs y = [n | (x,n) <- zip xs [0..], x == y]

-- 5.5.2 - Definir, por recursión, la función
--
-- posicionesR :: String -> Char -> [Int]
--
-- tal que (posicionesR xs y) es la lista de las posiciones del carácter y en la
-- cadena xs. Por ejemplo,
--
-- posicionesR "Salamamca" 'a' == [1,3,5,8]

posicionesR :: String -> Char -> [Int]
posicionesR xs y = posicionesAux xs y 0
    where
      posicionesAux [] y n = []
      posicionesAux (x:xs) y n | x == y = n : posicionesAux xs y (n+1)
                               | otherwise = posicionesAux xs y (n+1)

-- 5.5.3 - Comprobar con QuickCheck que ambas definiciones son equivalentes.

prop_posiciones :: String -> Char -> Bool
prop_posiciones xs y =
    posiciones xs y == posicionesR xs y

-- La comprobación es
-- 
-- ghci> quickCheck prop_posiciones
-- +++ OK, passed 100 tests.
--
-- 5.6 - Decidir si una cadena es subcadena de otra
-- -----------------------------------------------------------------------------
-- 5.6.1 - Definir, por recursión, la función
-- 
-- contieneR :: String -> String -> Bool
--
-- tal que (contieneR xs ys) se verifica si ys es una subcadena de xs. Por 
-- ejemplo,
--
-- contieneR "escasamente" "casa"  == True
-- contieneR "escasamente" "cante" == False
-- contieneR "" ""                 == True
--
-- Nota: Se puede usar la predefinida (isPrefixOf ys xs) que se verifica si ys 
-- es un prefijo de xs.

contieneR :: String -> String -> Bool
contieneR _ []  = True
contieneR [] ys = False
contieneR xs ys = isPrefixOf ys xs || contieneR (tail xs) ys

-- 5.6.2 - Definir, por comprensión, la función
-- 
-- contiene :: String -> String -> Bool
--
-- tal que (contiene xs ys) se verifica si ys es una subcadena de xs. Por 
-- ejemplo,
--
-- contiene "escasamente" "casa"     == True
-- contiene "escasamente" "cante"    == False
-- contiene "casado y casada" "casa" == True
-- contiene "" ""                    == True
--
-- Nota: Se puede usar la predefinida (isPrefixOf ys xs) que se verifica si ys 
-- es un prefijo de xs.

contiene :: String -> String -> Bool
contiene xs ys = sufijosComenzandoCon xs ys /= []

-- donde (sufijosComenzandoCon xs ys) es la lista de los sufijos de xs que 
-- comienzan con ys. Por ejemplo,
--
-- sufijosComenzandoCon "abacbad" "ba" == ["bacbad","bad"]

sufijosComenzandoCon :: String -> String -> [String]
sufijosComenzandoCon xs ys = [x | x <- sufijos xs, isPrefixOf ys x]

-- y (sufijos xs) es la lista de sufijos de xs. Por ejemplo,
--
-- sufijos "abc" == ["abc","bc","c",""]

sufijos :: String -> [String]
sufijos xs = [drop i xs | i <- [0..length xs]]

-- 5.6.3 - Comprobar con QuickCheck que ambas definiciones son equivalentes.

prop_contiene :: String -> String -> Bool
prop_contiene xs ys =
    contieneR xs ys == contiene xs ys

-- La comprobación es
-- 
-- ghci> quickCheck prop_contiene
-- +++ OK, passed 100 tests.
--
-- 5.7 - Condificación de mensajes
-- -----------------------------------------------------------------------------
-- Se desea definir una función que codifique mensajes tales como
--
-- eres lo que pinensas
--
-- del siguiente modo:
--
-- (a) se separa la cadena en la lista de sus palabras:
--
-- ["eres","lo","que","piensas"]
--
-- (b) se cuenta las letras de cada palabra:
--
-- [4,2,3,7]
--
-- (c) se une todas las palabras:
--
-- "eresloquepiensas"
--
-- (d) se reagrupa las letras de 4 en 4, dejando el último grupo con el resto:
--
-- ["eres","loqu","epie","nsas"]
--
-- (e) se invierte cada palabra:
--
-- ["sere","uqol","eipe","sasn"]
--
-- (f) se une todas las palabras
--
-- "sereuqoleipesasn"
--
-- (g) se reagrupan tal como indica la inversa de la lista del apartado (b):
--
-- ["sereuqo","lei","pe","sasn"]
--
-- (h) se crea una frase con las palabras anteriores separadas por un especio 
-- en blanco
--
-- "sereuqo lei pe sasn"
--
-- obteniendo así el mensaje codificado.
-- En los distintos apartados de esta sección se definirá el anterior proceso 
-- de codificación.
-- 5.7.1 - Definir la función
--
-- divide :: (a -> Bool) -> [a] -> ([a], [a])
--
-- tal que (divide p xs) es el par (ys,zs) donde ys es el mayor prefijo de xs
-- cuyos elementos cumplen p y zs es la lista de los restantes elementos de xs. 
-- Por ejemplo,
--
-- divide (< 3) [1,2,3,4,1,2,3,4] == ([1,2],[3,4,1,2,3,4])
-- divide (< 9) [1,2,3] == ([1,2,3],[])
-- divide (< 0) [1,2,3] == ([],[1,2,3])

divide :: (a -> Bool) -> [a] -> ([a], [a])
divide p xs = (takeWhile p xs, dropWhile p xs)

-- Es equivalente a la predefinida span

divide' :: (a -> Bool) -> [a] -> ([a], [a])
divide' = span

-- 5.7.2 - Definir la función
--
-- palabras :: String -> [String]
--
-- tal que (palabras cs) es la lista de las palabras de la cadena cs. Por 
-- ejemplo,
--
-- palabras "eres lo que piensas" == ["eres","lo","que","piensas"]

palabras :: String -> [String]
palabras [] = []
palabras cs = cs1 : palabras cs2
    where cs' = dropWhile (==' ') cs
          (cs1,cs2) = divide (/=' ') cs'

-- Es equivalente a la predefinida words

palabras' :: String -> [String]
palabras' = words

-- 5.7.3 - Definir la función
--
-- longitudes :: [[a]] -> [Int]
--
-- tal que (longitudes xss) es la lista de las longitudes de los elementos xss.
-- Por ejemplo,
--
-- longitudes ["eres","lo","que","piensas"] == [4,2,3,7]

longitudes :: [[a]] -> [Int]
longitudes = map length

-- 5.7.4 -- DEfinir la función
--
-- une :: [[a]] -> [a]
--
-- tal que (une xss) es la lista obtenida uniendo los elementos de xss. Por 
-- ejemplo,
--
-- une ["eres","lo","que","piensas"] == "eresloquepiensas"

une :: [[a]] -> [a]
une = concat

-- 5.7.5 - Definir la función
--
-- reagrupa :: [a] -> [[a]]
--
-- tal que (reagrupa xs) es la lista obtenida agrupando los elementos de xs de 
-- 4 en 4. Por ejemplo,
--
-- reagrupa "eresloquepiensas" == ["eres","loqu","epie","nsas"]
-- reagrupa "erestu" == ["eres","tu"]

reagrupa :: [a] -> [[a]]
reagrupa [] = []
reagrupa xs = take 4 xs : reagrupa (drop 4 xs)

-- 5.7.6 - Definir la función
--
-- inversas :: [[a]] -> [[a]]
--
-- tal que (inversas xss) es la lista obtenida invirtiendo los elementos de 
-- xss. Por ejemplo,
--
-- ghci> inversas ["eres","loqu","epie","nsas"]
-- ["sere","uqol","eipe","sasn"]
-- ghci> une (inversas ["eres","loqu","epie","nsas"])
-- "sereuqoleipesasn"

inversas :: [[a]] -> [[a]]
inversas = map reverse

-- 5.7.7 - Definir la función
-- 
-- agrupa :: [a] -> [Int] -> [[a]]
--
-- tal que (agrupa xs ns) es la lista obtenida agrupando los elementos de xs 
-- según las longitudes indicadas en ns. Por ejemplo,
--
-- ghci> agrupa "sereuqoleipesasn" [7,3,2,4]
-- ["sereuqo","lei","pe","sasn"]

agrupa :: [a] -> [Int] -> [[a]]
agrupa [] _      = []
agrupa xs (n:ns) = (take n xs) : (agrupa (drop n xs) ns)

-- 5.7.8 -Definir la función
--
-- frase :: [String] -> String
--
-- tal que (frase xs) es la frase obtenida las palabras de xs dejando un 
-- espacio en blanco entre ellas. Por ejemplo,
--
-- frase ["sereuqo","lei","pe","sasn"] == "sereuqo lei pe sasn"

frase :: [String] -> String
frase [x]    = x
frase (x:xs) = x ++ " " ++ frase xs
frase []     = []

-- La función frase es equivalente a unwords.

frase' :: [String] -> String
frase' = unwords

-- 5.7.9 - Definir la función
--
-- clave :: String -> String
--
-- que realice el proceso completo. Por ejemplo,
--
-- clave "eres lo que piensas" == "sereuqo lei pe sasn"

clave :: String -> String
clave xss = frase (agrupa (une (inversas (reagrupa (une ps))))
                          (reverse (longitudes ps)))
    where ps = palabras xss

-- 5.8 - Número de ceros finales
-- -----------------------------------------------------------------------------
-- 5.8.1 - Definir, por recursión, la función
--
-- ceros :: Int -> Int
-- 
-- tal que (ceros n) es el número de ceros en los que termina el número n. Por 
-- ejemplo,
--
-- ceros 30500 == 2
-- ceros 30501 == 0

ceros :: Int -> Int
ceros n | n `rem` 10 == 0 = 1 + ceros (n `div`10)
        | otherwise       = 0

-- 5.8.2 - Definir, sin recursión, la función
--
-- ceros' :: Int -> Int
--
-- tal que (ceros' n) es el número de ceros en los que terminar el númerno n. 
-- Por ejemplo,
--
-- ceros' 30500 == 2
-- ceros' 30501 == 0

ceros' :: Int -> Int
ceros' n = length (takeWhile (=='0') (reverse (show n)))