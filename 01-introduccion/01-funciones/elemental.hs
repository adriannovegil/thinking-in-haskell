module Main where

-- 01 - Media de 3 números.
-- -----------------------------------------------------------------------------
-- Definir la función media3 tal que (media3 x y z) es la media aritmética de
-- los números x, y y z. Por ejemplo,
--
-- media3 1 3 8 == 4.0
-- media3 (-1) 0 7 == 2.0
-- media3 (-3) 0 3 == 0.0

media3 x y z = (x + y + z) / 3

-- 02 - Suma de euros de una colección de monedas.
-- -----------------------------------------------------------------------------
-- Definir la función sumaMonedas tal que (sumaMonedas a b c d e) es la suma de
-- los euros correspondientes a a monedas de 1 euro, b de 2 euros, c de 5 euros,
-- d de 10 euros y e de 20 euros. Por ejemplo,
--
-- sumaMonedas 0 0 0 0 1 == 20
-- sumaMonedas 0 0 8 0 3 == 100
-- sumaMonedas 1 1 1 1 1 == 38

sumaMonedas a b c d e = 1 * a + 2 * b + 5 * c + 10 * d + 20 * e

-- 03 - Volumen de una esfera.
-- -----------------------------------------------------------------------------
-- Definir la función volumenEsfera tal que (volumenEsfera r) es el volumen de
-- la esfera de radio r. Por ejemplo,
--
-- volumenEsfera 10 == 4188.790204786391

volumenEsfera r = (4 / 3) * pi * r^3

-- 04 - Área de una corona circular.
-- -----------------------------------------------------------------------------
-- definir la función areaDeCoronaCircular tal que (areaDeCoronaCircular r1 r2)
-- es el área de una corona circular de radio interior r1 y radio exterior r2.
-- Por ejemplo,
--
-- areaDeCoronaCircular 1 2 == 9.42477796076938
-- areaDeCoronaCircular 2 5 == 65.97344572538566
-- areaDeCoronaCircular 3 5 == 50.26548245743669

areaDeCoronaCircular r1 r2 = pi * (r2^2 - r1^2)

-- 05 - Última ficra de un número.
-- -----------------------------------------------------------------------------
-- Definirla la función ultimaCifra tal que (ultimaCifra x) es la última cifra
-- del número x. Por ejemplo,
--
-- ultimaCifra 325 == 5

ultimaCifra x = rem x 10

-- 06 - Máximo de 3 elementos.
-- -----------------------------------------------------------------------------
-- Definir la función maxTres tal que (maxTres x y z) es el máximo de x, y y z.
-- Por ejemplo,
--
-- maxTres 6 2 4 == 6
-- maxTres 6 7 4 == 7
-- maxTres 6 7 9 == 9

maxTres x y z = max x (max y z)

-- 07 - Disyunción excluyente.
-- -----------------------------------------------------------------------------
-- La disyunción excluyente xor de dos fórmulas se verifica si una es verdadera
-- y la otra es falsa.

-- 7.1 - Definir la función xor1 que calcule la disyunción excluyente a partir
-- de la tabla de verdad. Usar 4 ecuaciones, una por cada línea de la tabla.

xor1 True True = False
xor1 True False = True
xor1 False True = True
xor1 False False = False

-- 7.2 - Definir la función xor2 que calcule la disyunción excluyente a partir
-- de la tabla de verdad y patrone. Usar 2 ecuaciones, una por cada valor del
-- primer argumento.

xor2 True y = not y
xor2 False y = y

-- 7.3 - Definir la función xor3 que calcule la disyunción ecluyente a partir
-- de la disyunción (||), conjunción (&&) y negación (not). Usar 1 ecuación.

xor3 x y = (x || y) && not (x && y)

-- 7.4 - Definir la función xor4 que calcule la disyunción excluyente a partir
-- de la desigualdad (/=). Usar 1 ecuación.

xor4 x y = x /= y

-- 08 - Rotación de listas.
-- -----------------------------------------------------------------------------

-- 8.1 - Definir la función rota1 tal que (rota1 xs) es la lista obtenida
-- poniendo el primer elemento de xs al final de la lista. Por ejemplo,
--
-- rota1 [3,2,5,7] == [2,5,7,3]

rota1 xs = tail xs ++ [head xs]

-- 8.2 definir la función rota tal que (rota n xs) es la lista obtenida
-- poniendo los n primeros elemntos de xs al final de la lista. por ejemplo,
--
-- rota 1 [3,2,5,7] == [2,5,7,3]
-- rota 2 [3,2,5,7] == [5,7,3,2]
-- rota 3 [3,2,5,7] == [7,3,2,5]

rota n xs = drop n xs ++ take n xs

-- 09 - Ranggo de una lista.
-- -----------------------------------------------------------------------------
-- Definir la función rango tal que (rango xs) es la lista formada por el menor
-- y mayor elemento de xs. Por ejemplo,
--
-- rango [3,2,7,5] == [2,7]
--
-- indicación: Se pueden usar min y max

rango xs = [minimum xs, maximum xs]

-- 10 - Reconocimiento de palíndromos.
-- -----------------------------------------------------------------------------
-- Definir la función palinddromo tal que (palindromo xs) se verifica si xs es
-- un palíndromo; es decir, es lo mismo leer xs de izquierda a derecha que de
-- derecha a izquierda. Por ejemplo,
--
-- palindromo [3,2,5,2,3] == True
-- palindromo [3,2,5,6,2,3] == False

palindromo xs = xs == reverse xs

-- 11 - Elementos interiores de una lista.
-- -----------------------------------------------------------------------------
-- Definir la función interior ral que (interior xs) es la lista obtenida
-- eliminando los extremos de la lista xs. Por ejemplo,
--
-- interior [2,5,3,7,3] == [5,3,7]
-- interior [2..7] == [3,4,5,6]

interior xs = tail(init xs)

-- 12 - Finales de una lista.
-- -----------------------------------------------------------------------------
-- Definir la función finales tal que (finales n xs) es la lista formada por los
-- n finales elementos de xs. Por ejemplo,
--
-- finales 3 [2,5,4,7,9,6] == [7,9,6]

finales n xs = drop (length xs - n) xs

-- 13 - Segmentos de una lista.
-- -----------------------------------------------------------------------------
-- Definir la función segmento tal que (segmento m n xs) es la lista de los
-- elementos de xs comprendidos entre las posiciones m y n. Por ejemplo,
--
-- segmento 3 4 [3,4,1,2,7,9,0] == [1,2]
-- segmento 3 5 [3,4,1,2,7,9,0] == [1,2,7]
-- segmento 5 3 [3,4,1,2,7,9,0] == []

segmento m n xs = drop (m - 1) (take n xs)

-- 14 - Extremos de una lista.
-- -----------------------------------------------------------------------------
-- Definir la función extremos tal que (extremos n xs) es la lista formada por
-- los n primeros elementos de xs y los n finales elemntos de xs. Por ejemplo,
--
-- extremos 3 [2,6,7,1,2,4,5,8,9,2,3] == [2,6,7,9,2,3]

extremos n xs = take n xs ++ drop (length xs - n) xs

-- 15 - Mediano de 3 números.
-- -----------------------------------------------------------------------------
-- definir la función mediano tal que (mediano x y z) es el número mediano de
-- los tres números x,y y z. Por ejemplo,
--
-- mediano 3 2 5 == 3
-- mediano 2 4 5 == 4
-- mediano 2 6 5 == 5
-- mediano 2 6 6 == 6

mediano x y z = x + y + z - minimum [x,y,z] - maximum[x,y,z]

-- 16 - Igualdad y diferencia de 3 elementos.
-- -----------------------------------------------------------------------------

-- 16.1 Definir la función tresIguales tal que (tresIguales x y z) se verifica
-- si los elementos x, y y z son iguales. Por ejemplo,
--
-- tresIguales 4 4 4 == true
-- tresIguales 4 3 4 == false

tresIguales x y z = x == y && y == z

-- 16.2 Definir la función tresDiferente tal que (tresDiferentes x y z) se
-- verifica si los elementos x, y y z son distintos. Por ejemplo,
--
-- tresDiferentes 3 5 2 == true
-- tresDiferentes 3 5 3 == false

tresDiferentes x y z = x /= y && x /= z && y /= z

-- 17 - Igualdad de 4 elementos.
-- -----------------------------------------------------------------------------
-- Definir la función cuatroIguales tal que (cuatroIguales x y z u) se verifica
-- si los elementos x, y, z y u son iguales. Por ejemplo,
--
-- cuatroIguales 5 5 5 5 == true
-- cuatroIguales 5 5 4 5 == false
--
-- Indicación: Usar la función tresIguales.

cuatroIguales x y z u = x == y && tresIguales y z u

-- 18 - Propiedad triangular.
-- -----------------------------------------------------------------------------
-- Las longitudes de los lados de un triángulo no pueden ser cualesquiera. Para
-- que pueda construirse el triángulo, triene que cumplirse la propiedad
-- triangular; es decir, longitud de cada lado tiene que se menor que la suma de
-- los otros dos lados.
-- Definir la función triangular tal que (triangular a b c) se verifica si a, b
-- y c cumplen la propiedad triangular. Por ejemplo,
--
-- triangular 3 4 5 == true
-- triangular 30 4 5 == false
-- triangular 3 40 5 == false
-- triangular 3 4 50 == false



-- 19 - División segura.
-- -----------------------------------------------------------------------------
-- Definir la función divisionSegura ral que (divisionsegura x y ) es x/y si y
-- no es cero, y 9999 en caso contrario. Por ejemplo,
--
-- divisionSegura 7 2 = 3.5
-- divisionSegura 7 0 = 9999.0



-- 20 - Módulo de un vector.
-- -----------------------------------------------------------------------------
-- Definir la función modulo tal que (modulo v) es el módulo del vector v. Por
-- ejemplo,
--
-- modulo (3,4) == 5.0



-- 21 - Rectángulo de área máxima.
-- -----------------------------------------------------------------------------
-- Las dimensiones de los rectángulos pueden representarse por pares; por
-- ejemplo, (5,3) representa a un rectángulo de base 5 y altura 3. Definir la
-- función mayorRectangulo tal que (mayorRectangulo r1 r2) es el rectángulo de
-- mayor área entre r1 y r2. Por ejemplo,
--
-- mayorRectangulo (4,6) (3,7) == (4,6)
-- mayorRectangulo (4,6) (3,8) == (4,6)
-- mayorRectangulo (4,6) (3,9) == (3,9)



-- 22 - Puntos de plano.
-- -----------------------------------------------------------------------------
-- Los puntos se pueden representar por un par de números que son sus
-- coordenadas.

-- 22.1 - Cuadrante de un punto.
-- Definir la función cuadrante tal que (cuadrante p) es el cuadrante del punto
-- p (se supone que p no está sobre los ejes). Por ejemplo,
--
-- cuadrante (3,5) == 1
-- cuadrante (-3,5) == 2
-- cuadrante (-3,-5) == 3
-- cuadrante (3,-5) == 4



-- 22.2 - Intercambio de coordenadas
-- Definir la función intercambia tal que (intercambia p) es el punto obtenido
-- intercambiando las coordenadas del punto p. Por ejemplo,
--
-- intercambia (2,5) == (5,2)
-- intercambia (5,2) == (2,5)



-- 22.3 - Punto simétrico
-- Definir la función simétricoH tal que (simetricoH p) es el punto simétrico
-- de p respecto del eje horizontal. Por ejemplo,
--
-- simetricoH (2,5) == (2,-5)
-- simetricoH (2,-5) == (2,5)



-- 22.4 - Distancia entre dos puntos
-- Definir la función distancia tal que (distancia p1 p2) es la distancia
-- entre los puntos p1 y p2. Por ejemplo,
--
-- distancia (1,2) (4,6) == 5.0



-- 22.5 - Punto medio entre otros dos
-- Definir la función puntoMedio tal que (puntoMedio p1 p2) es el punto medio
-- entre los puntos p1 y p2. Por ejemplo,
--
-- puntoMedio (0,2) (0,6) == (0.0,4.0)
-- puntoMedio (-1,2) (7,6) == (3.0,4.0)



-- 23 - Números complejos.
-- -----------------------------------------------------------------------------
-- Los números complejos pueden representarse mediante pares de números
-- complejos. por ejemplo, el número 2+5i puede representarse mediante el par
-- (2,5).

-- 23.1 - Suma de dos números complejos
-- Definir la función sumaComplejos ral que (sumaComplejos x y) es la suma de
-- los números complejos x e y. Por ejemplo,
--
-- sumaComplejos (2,3) (5,6) == (7,9)



-- 23.2 - Producto de dos números complejos
-- Definir la función productoComplejos tal que (productoComplejos x y) es el
-- producto de los números complejos x e y. Por ejemplo,
--
-- productoComplejos (2,3) (5,6)



-- 23.3 - Conjugado de un número complejo
-- Definir la función conjugado tal que (conjugado z) es el conjugado del
-- número complejo z. Por ejemplo,
--
-- conjugado (2,3) == (2,-3)



-- 24 - Intercalación de pares.
-- -----------------------------------------------------------------------------
-- Definir la función intercala que reciba dos listas xs e ys de dos elementos
-- cada una, y devuelva una lista de cuatro elementos, construida intercambiando
-- los elementos de xs e ys. Por ejemplo,
--
-- intercala [1,4] [3,2] == [1,3,4,2]



-- 25 - Permutación cíclica de una lista.
-- -----------------------------------------------------------------------------
-- Definir una función ciclo que permute cíclicamente los elementros de una
-- lista, pasando el último elemento al principio de la lista. Por ejemplo,
--
-- ciclo [2,5,7,9] == [9,2,5,7]
-- ciclo [] == []
-- ciclo [2] == [2]



-- 26 - Mayor número de 2 cifras con dos dígitos dados.
-- -----------------------------------------------------------------------------
-- Definir la función numeroMayor tal que (numeroMayor x y) es el mayor número
-- de dos cifras que puede construirse con los dígitos x e y. Por ejemplo,
--
-- numeroMayor 2 5 == 52
-- numeroMayor 5 2 == 52



-- 27 - Número de raíces de una ecuación cuadrática.
-- -----------------------------------------------------------------------------
-- Definir la función numeroDeRaices tal que (numeroDeRaices a b c) es el número
-- de raíces reales de la ecuación ax^2+bx+c = 0. Por ejemplo,
--
-- numeroDeRaices 2 0 3 == 0
-- numeroDeRaices 4 4 1 == 1
-- numeroDeRaices 5 23 12 == 2



-- 28 - Raíces de las ecuaciones cuadráticas.
-- -----------------------------------------------------------------------------
-- Definir la función raices de forma que (raices a b c) devuelve la lista de
-- las raices reales de la ecuación ax^2+bx+c = 0. Por ejemplo,
--
-- raices 1 (-2) 1 == [1.0,1.0]
-- raices 1 3 2 == [-1.0,-2.0]



-- 29 - Área de un triángulo mediante la formula de Herón.
-- -----------------------------------------------------------------------------
-- En geometría, la formula de Herón, descubierta por Herón de Alejandría, dice
-- que el área de un triángulo cuyos lados miden a, b y c es
-- sqrt(s(s-a)(s-b)(s-c)), donde s es el semiperímetro s = (a+b+c)/2
-- Definir la función area tal que (area a b c) es el área de un triángulo de
-- lados a, b y c. Por ejemplo,
--
-- area 3 4 5 == 6.0



-- 30 - Números racionales como pares de enteros.
-- -----------------------------------------------------------------------------
-- Los números racionales pueden representarse mediante pares de números
-- enteros. Por ejemplo, el número 2/5 puede representarse mediante el par (2,5)

-- Función para el cálculo de mínimo como un divisor



-- 30.1 - Forma reducida de un número racional.
-- Definir la función formaReducida ral que (formaReducida x) es la forma
-- reducida del número racional x. Por ejemplo,
--
-- formaReducida (4,10) == (2,5)



-- 30.2 - Suma de dos números racionales.
-- Definir la función sumaRacional tal que (sumaRacional x y) es la suma de
-- los números racionales x e y. Por ejemplo,
--
-- sumaRacional (2,3) (5,6) == (3,2)



-- 30.3 - Producto de dos números racionales.
-- Definir la función productoRacional tal que (productoRacional x y) es el
-- producto de los números racionales x e y. Por ejemplo,
--
-- productoRacional (2,3) (5,6) == (5,9)



-- 30.4 - Igualdad de números racionales.
-- Definir la función igualdadRacional tal que (igualdadRacional x y) se
-- verifica si los números racionales x e y son iguales. Por ejemplo,
--
-- igualdadRacional (6,9) (10,15) == true
-- igualdadRacional (6,9) (11,15) == false
