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