--------------------------------------------------------------------------------
-- Universidad Nacional Autónoma de México, Facultad de Ciencias              --
-- Lógica Computacional 2017-2                                                --
-- Práctica 3: Relación de alpha-equivalencia y Algoritmo de Martelli         --
-- Montanari                                                                  --
--                                                                            --
-- Descripción:                                                               --
-- Módulo para trabajar con la sintaxis y semántica de las expresiones del    --
-- lenguaje FORM y unificación                                                --
--                                                                            --
--  Profesor Pilar Selene Linares Arévalo                                     --
--  Ayudante Uriel Agustín Ochoa González                                     --
--  Ayudante Diego Murillo Albarran                                           --
-- Ayud.Lab. Manuel Soto Romero                                               --
-- Ayud.Lab. Víctor Zamora Gutiérrez                                          --
--------------------------------------------------------------------------------
module PrimerOrden where

-- Sinónimo para representar los nombres de variables y funciones.
type Nombre = String

-- Sinónimo para representar sustituciones.
type Sust = [(Nombre,Termino)]

-- Gramática para representar términos.
data Termino = V Nombre
             | F Nombre [Termino] deriving (Eq)

-- Gramática para representar fórmulas de la Lógica de Primer Orden.
data FORM = TrueF
             | FalseF
             | Pr Nombre [Termino]
             | Eq Termino Termino
             | Neg FORM
             | Conj FORM FORM
             | Disy FORM FORM
             | Impl FORM FORM
             | Equi FORM FORM
             | PT Nombre FORM
             | EX Nombre FORM deriving(Eq)


-- Función que regresa las variables libres de una formula
-- fV:: FORM -> [Nombre]


-- Función que verifica si dos fórmulas son alpha-equivalentes.
vAlfaEq :: FORM -> FORM -> Bool
vAlfaEq f g = error "Función no implementada"

-- Función que renombra las variables ligadas de una fórmula de manera que las
-- listas de variables libres y ligadas que sean ajenas. Estos es un caso 
-- particular de la siguiente función.
renVL :: FORM -> FORM
renVL f = error "Función no implementada"

-- Función que renombra la variables ligadas de una fórmula de forma que sus 
-- nombres sean ajenos a los de una lista dada.
renVLconj :: FORM -> [Nombre] -> FORM
renVLconj f ls = error "Función no implementada"

-- Función que implementa la sustitución en fórmulas usano la 
-- alpha-equivalencia.
apsubF2 :: FORM -> Sust -> FORM
apsubF2 f s = error "Función no implementada"

-- Función que dada una sustitución, elimina de ella los pares con componentes
-- iguales correspondientes a sustituciones de la forma x:=x.
simpSus :: Sust -> Sust
simpSus s = error "Función no implementada"

-- Función que dadas dos sustituciones devuelve su composición.
compSus :: Sust -> Sust -> Sust
compSus s t = error "Función no implementada"

-- Función que dados dos términos devuelve una lista de sustituciones de tal
-- forma que:
-- · Si t1, t2 no son unificables la lista es vacía.
-- · Si sí lo son, la lista contiene como único elemento al unificador 
--   correspondiente.
unifica :: Termino -> Termino -> [Sust]
unifica t s = error "Función no implementada"

-- Función que implementa el caso general para unificar un conjunto (lista)
-- W = {t1,..,tn}
unificaConj :: [Termino] -> [Sust]
unificaConj ls = error "Función no implementada"
