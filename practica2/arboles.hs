--------------------------------------------------------------------------------
-- Universidad Nacional Autónoma de México, Facultad de Ciencias              --
-- Lógica Computacional 2017-2                                                --
-- Práctica 2: Gramáticas / Sintaxis y semántica del lenguaje PROP.           --
--                                                                            --
-- Descripción:                                                               --
-- Módulo para trabajar con árboles cuyas hojas son los únicos nodos con      --
-- información.                                                               --
--                                                                            --
--  Profesor Pilar Selene Linares Arévalo                                     --
--  Ayudante Uriel Agustín Ochoa González                                     --
--  Ayudante Diego Murillo Albarran                                           --
-- Ayud.Lab. Manuel Soto Romero                                               --
-- Ayud.Lab. Víctor Zamora Gutiérrez                                          --
--------------------------------------------------------------------------------

module ARBOLESP2 where

-- Gramática para representar a los árboles binarios
data AB a = Hoja a
          | Mkt (AB a) (AB a) deriving (Show, Eq)

-- Función que regresa el número de hojas del árbol.
nh :: AB a -> Int
nh (Hoja a) = 1
nh (Mkt a b) = nh a + nh b

-- Función que regresa el número de nodos internos del árbol.
nni :: AB a -> Int
nni (Hoja a) = 0
nni (Mkt a b) = 1 + nni a + nni b

-- Función que determina si un elemento está contenido en el árbol.
elemA :: Eq a => AB a -> a -> Bool
elemA (Hoja x) a | x == a = True 
elemA (Hoja x) a | x /= a = False  
elemA (Mkt x y) a = elemA x a || elemA y a

-- Función que toma un árbol y regresa una lista con los elementos en la forma
-- inorder.
inorderA :: AB a -> [a]
inorderA (Hoja a) = [a]
inorderA (Mkt (Hoja a)(Mkt t1 t2)) = inorderA t1 ++ [a] ++ inorderA t2

-- Función que toma un elemento y lo agrega al árbol.
-- Función que toma un elemento y lo agrega al árbol.
agregaHoja :: AB a -> a -> AB a
agregaHoja (Hoja a) e = (Mkt (Hoja a)(Hoja e))
agregaHoja (Mkt t1 t2) e = (Mkt (Mkt t1 (Hoja e))t2)


-- Función que dado un árbol y una función, aplica la misma a cada elemento del
-- árbol.
mapA :: AB a -> (a -> b) -> AB b
mapA (Hoja x) f = Hoja (f x)
mapA (Mkt x y) f = Mkt (mapA x f) (mapA y f)

-- Función que regresa la profundidad del árbol.
profundidad :: AB a -> Int
profundidad (Hoja a) = 1
profundidad (Mkt (t1)(t2)) = max(profundidad t1) (profundidad t2) + 1