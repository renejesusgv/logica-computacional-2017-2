--------------------------------------------------------------------------------
-- Universidad Nacional Autónoma de México, Facultad de Ciencias              --
-- Lógica Computacional 2017-2                                                --
-- Práctica 2: Gramáticas / Sintaxis y semántica del lenguaje PROP.           --
--                                                                            --
-- Descripción:                                                               --
-- Módulo para trabajar con expresiones aritméticas. Las expresiones trabajan --
-- con números naturales. Las operaciones que se tienen son: suma, resta,     --
-- multiplicación, división y se tiene una opción para parentizar una         --
-- expresión                                                                  --
--                                                                            --
--  Profesor Pilar Selene Linares Arévalo                                     --
--  Ayudante Uriel Agustín Ochoa González                                     --
--  Ayudante Diego Murillo Albarran                                           --
-- Ayud.Lab. Manuel Soto Romero                                               --
-- Ayud.Lab. Víctor Zamora Gutiérrez                                          --
--------------------------------------------------------------------------------

module EAP2 where

import Data.Maybe

-- Gramática para representar a los números naturales
data Nat = Cero
         | Suc Nat deriving(Show, Eq)

-- Gramática para representar símbolos de variables
data Id = A|B|C|D|E|F|G|H|I|J|K|L|M|N|O|P|Q|R|S|T|U|V|W|X|Y|Z deriving(Show,Eq)

-- Gramática para representar a las expresiones aritméticas
data EA = Var Id
        | Cte Nat
        | Sum EA EA
        | Res EA EA
        | Mul EA EA
        | Div EA EA
        | Paren EA deriving(Eq)
 
-- Sinónimo para representar al ambiente de evaluación
type Env = [(Id, Int)]

-- Hace parte de la familia Show al tipo Booleano.
instance Show EA where
   show exp = showEA exp

aEntero :: Nat -> Int
aEntero Cero = 0
aEntero (Suc n) = 1 + aEntero n

-- Función que dada una expresión aritmética, devuelve su representación como
-- cadena.
showEA :: EA -> String
showEA (Var id) = show id
showEA (Cte nat) = show (aEntero nat) 
showEA (Sum x y) = showEA x ++ " + " ++ showEA y
showEA (Res x y) = showEA x ++ " - " ++ showEA y
showEA (Mul x y) = showEA x ++ " * " ++ showEA y
showEA (Div x y) = showEA x ++ " / " ++ showEA y
showEA (Paren s) = "(" ++ showEA s ++ ")"

-- Función que dada una expresión y un ambiente de evaluación, devuelve el 
-- resultado de evaluar dicha expresión como el entero que la representa.
evalua :: EA -> Env -> Int
evalua (Var id) [] = error "No se encontró la variable en el ambiente" 
evalua (Var id) l = sustituyeEA id l
evalua (Cte nat) l = aEntero nat
evalua (Sum x y) l = (evalua x l) + (evalua y l)
evalua (Res x y) l = (evalua x l) - (evalua y l)
evalua (Mul x y) l = (evalua x l) * (evalua y l)
evalua (Div x y) l = div (evalua x l) (evalua y l)

sustituyeEA :: Id -> Env -> Int
sustituyeEA x ((id,val):ys)
  | x == id = val
  | otherwise = sustituyeEA x ys