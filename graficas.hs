--------------------------------------------------------------------------------
-- Universidad Nacional Autónoma de México, Facultad de Ciencias              --
-- Lógica Computacional 2017-2                                                --
-- Práctica 3: Resolución binaria           --
--                                                                            --
-- Descripción:                                                               --
-- Módulo para trabajar con gráficas. Incluye funciones relacionadas con el   --
-- problema SAT.                                                              --
--                                                                            --
--  Profesor Pilar Selene Linares Arévalo                                     --
--  Ayudante Uriel Agustín Ochoa González                                     --
--  Ayudante Diego Murillo Albarran                                           --
-- Ayud.Lab. Manuel Soto Romero                                               --
-- Ayud.Lab. Víctor Zamora Gutiérrez                                          --
--------------------------------------------------------------------------------
module Graficas where

-- Sinónimo para representar a los vértices. Para fines prácticos, supondremos
-- que la gráfica almacena datos de tipo entero. De esta forma, un vértice es
-- simplemente un Int.
type Vertice = Int
 
-- Sinónimo para representar las adyacencias de un vértice. Una adyacencia es 
-- una tupla formada por un vértice y la lista de vértices con los que se 
-- conecta el primero.
type Adyacencia = (Vertice, [Vertice])

-- Sinónimo para representar a las gráficas. Siguiendo la idea anterior. Una
-- gráfica es vista como una lsita de adyacencias.
type Grafica = [Adyacencia]

-- Función que dada una gráfica regresa una lista con los vértices que la 
-- conforman (Enteros).
vertices :: Grafica -> [Vertice]
vertices [] = []
vertices ((v, b):l) = [v]++ vertices l

-- Función que dada una gráfica determina si es conexa o no.

esConexa :: Grafica -> Bool
esConexa g = if comparaList (vertices g) (aplicar (verticesAd g)) then True else False

aplicar::[[Vertice]]-> [Vertice]
aplicar [] = []
aplicar (x:xs) = if compara x (head xs) then une x (head xs) else []

--Crea una lista de los vértices adyacentes a un vértice más el mismo.
verticesAd::Grafica->[[Vertice]]
verticesAd [] = []
verticesAd ((v, b):l) = [[v]++b]++verticesAd l

--Busca si al menos un elemento de una lista aparece en otra.
compara::[Vertice]->[Vertice]->Bool
compara [] l2 = False
compara (x:xs) l2 
	| elem x l2 = True 
	| otherwise = compara xs l2

--Une un par de listas de vértices.
une::[Vertice]->[Vertice]->[Vertice]
une x y = elimRepetidos (x++y)

--Crea lista con una sola aparición de cada vértice. 
elimRepetidos:: [Vertice]->[Vertice]
elimRepetidos [] = []
elimRepetidos (x:xs)= if elem x xs then elimRepetidos xs else [x]++elimRepetidos xs 

--Compara si todos los elementos de una lista están en otra lista.
comparaList:: [Vertice]->[Vertice]->Bool
comparaList [] l2 = True
comparaList (x:xs) l2 
	| elem x l2 = comparaList xs l2
	| otherwise = False 

--Función que dada una gráfica determina si es completa o no.
esCompleta :: Grafica -> Bool
esCompleta g 
	| comparaAris (cadaVertice (vertices g) (vertices g)) (aristasG (verticesAd g)) ==False = False
	| otherwise = True

--Crea todas las aristas de una gráfica completa
cadaVertice:: [Vertice]->[Vertice]-> [(Vertice,Vertice)]
cadaVertice [] l= []
cadaVertice (x:xs) l = aristasC x l ++ cadaVertice xs l

--Crea aristas doblemente dirigidas
aristasC:: Vertice->[Vertice]-> [(Vertice,Vertice)]
aristasC v [] = []
aristasC v (x:xs) 
	| v == x =  aristasC v xs 
	| otherwise = [(v,x)]++ [(x,v)]++ aristasC v xs

--Crea las aristas de una gráfica 
aristasG:: [[Vertice]]-> [(Vertice,Vertice)]
aristasG [] = []
aristasG (x:xs) = cadaVertice2 x x ++ aristasG xs

--Aplica las aristas doblemente dirigidas por cada adyacencia de un vértice.
cadaVertice2:: [Vertice]->[Vertice]-> [(Vertice,Vertice)]
cadaVertice2 [] l= []
cadaVertice2 (x:xs) l = aristasC x l 

--Compara si todas las aristas de una lista están en otra lista.
comparaAris:: [(Vertice,Vertice)]->[(Vertice,Vertice)]->Bool
comparaAris [] l2 = True
comparaAris (x:xs) l2 
	| elem x l2 = comparaAris xs l2
	| otherwise = False
