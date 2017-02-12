module Practica1 where

-- Función que encuentra la derivada f'(v) de la ecuación f(x) = ax²+bx+c. El 
-- primer argumento de la función corresponderá al valor de a, el segundo al 
-- valor de b, el tercero al valor de c y el cuarto al valor de v que es el 
-- valor de evaluación.
deriva :: Int -> Int -> Int -> Int -> Int
deriva a b c v = 2*a*v + b 

-- Función para calcular el área de un cilindro dada la altura y el diámetro
-- como primer y segundo parámetro respectivamente.
areaCilindro :: Float -> Float -> Float
areaCilindro d h =  2 * pi * r * (r +h)
	where r = d/2

-- Función para calcular el volumen de un cilindro dada la altura y el diámetro
-- como primer y segundo parámetro respectivamente.
volumenCilindro :: Float -> Float -> Float
volumenCilindro d h = pi * (r**2) * h
	where r = d/2

-- Función que recibe tres parámetros, el primero indica la operación que se va
-- a realizar con los otros dos parámetros, las posibles operaciones son:
--
-- 's' = devuelve el segundo parámetro
-- 't' = devuelve el tercer parámetro
-- 'a' = suma
-- 'r' = resta
-- 'p' = multiplicación
-- 'd' = división entera
-- 'e' = potencia (el segundo parámetro elevado al tercero)
aplicaOperacion :: Char -> Int -> Int -> Int
aplicaOperacion op l r 
	| op == 's' = l
	| op == 't' = r
	| op == 'a' = l + r
	| op == 'r' = l - r
	| op == 'p' = l * r
	| op == 'd' = (div l r)
	| op == 'e' = l^r
	| otherwise = error "Operacion desconocida"

-- Función recursiva que calcula una aproximación con un número entero a la raíz
-- cuadrada.
raizEntera :: Int -> Int
raizEntera n = error "Función implementada"

-- Función recursiva que devuelve la suma de los primeros n números naturales.
sumaNat :: Int -> Int
sumaNat n = if n == 0 then 0 else n + sumaNat (n-1)

-- Función recursiva que devuelve la longitud de un número entero.
longitud :: Int -> Int
longitud n = error "Función no implementada"

-- Función que regresa una lista con los n primeros números de tribonacci 
-- iniciando con 0, 0, 1.
tribonaccies :: Int -> [Int]
tribonaccies l = map fibonacci [0,1..l-1] 

fibonacci :: Int->Int
fibonacci n
	| n == 0 || n == 1 = 0
	| n == 2 = 1
	| otherwise = fibonacci(n-1) + fibonacci (n-2) + fibonacci (n-3)


-- Función que dada una lista elimina los elementos duplicados adyacentes de una
-- lista dejando únicamente una aparición de cada elemento. La implementación de
-- esta función usa foldr.
elimDup :: [a] -> [a]
elimDup ls = error "Función no implementada"

-- Función que dada una función de comparación y una lista como parámetros,
-- devuelve el elemento maximal de la lista para esa función de comparación. La
-- implementación de esta función usa foldl.
maximal :: (a -> a -> a) -> [a] -> a
maximal f l =error "Función no implementada"

-- foldl (f) [] l

-- Función que regresa la reversa de una lista.
reversa :: [a] -> [a]
reversa [] =[]
reversa (x:xs) = (reversa xs)++[x]  
--ls = error "Función no implementada"

-- Función que devuelve una lista con los elementos que cumplen con el predicado
-- recibido como parámetro
filtra :: (a -> Bool) -> [a] -> [a]
filtra p l = error "Función no implementada"

-- Función que toma una lista como parámetro y regresa otra lista con los 
-- elementos que aparecen una única vez en la original.
unicaVez :: [a] -> [a]
unicaVez l = error "Función no implementada"

-- Función que recibe una lista y regresa una lista de pares (k, x), donde k es
-- el número de apariciones consecutivas de x en la lista recibida.
apariciones :: [a] -> [(Int, a)]
apariciones ls = error "Función no implementada"

-- Función que dada una lista de la forma [a0,a1,a2, ... , am,an,ao,ap] devuelve
-- una lista de pares cuyos elementos son (a0,ap) (a1,ao) (a2 an). Se debe 
-- asegurar que la lista recibida siemre sea de longitud par.
empareja :: [a] -> [(a,a)]
empareja ls = error "Función no implementada"


-- AGREGA AQUÍ LA DEFINICIÓN DE LAS LISTAS POR COMPRENSIÓN

listaComprension1::[Int]
listaComprension1 =[(2^x)-1 | x <- [0,1..6]]

listaComprension2::[(Int,Int)]
listaComprension2 = [(x, x+1) | x <- [3,7..99]]
