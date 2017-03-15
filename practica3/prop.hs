--------------------------------------------------------------------------------
-- Universidad Nacional Autónoma de México, Facultad de Ciencias              --
-- Lógica Computacional 2017-2                                                --
-- Práctica 3: Resolución binaria           --
--                                                                            --
-- Descripción:                                                               --
-- Módulo para trabajar con la sintaxis y semántica de las expresiones del    --
-- lenguaje PROP y resoución binaria.                                                             --
--                                                                            --
--  Profesor Pilar Selene Linares Arévalo                                     --
--  Ayudante Uriel Agustín Ochoa González                                     --
--  Ayudante Diego Murillo Albarran                                           --
-- Ayud.Lab. Manuel Soto Romero                                               --
-- Ayud.Lab. Víctor Zamora Gutiérrez                                          --
--------------------------------------------------------------------------------

module PROPP3 where

-- Sinónimo para representar estados
type Estado = (VarP, Booleano)

-- Gramática para contantes lógicas
data Booleano = V | F deriving (Show,Eq)

-- Gramática para variables proposicionales
data VarP = A|B|C|D|E|G|H|I|J|K|L|M|N|O|P|Q|R|S|T|U|W|X|Y|Z deriving (Show, Eq)

-- Gramática para representar átomos
data Atomo = Var VarP | Cte Booleano deriving (Eq)

-- Gramática para representar a los operadores binarios.
data OpBin = Conj | Disy | Impl | Syss deriving(Eq)

-- Gramática para representar expresiones del lenguaje Prop.
data Prop = FA Atomo
          | Neg Prop
          | Op Prop OpBin Prop deriving(Eq)

-- AQUÍ HAY QUE DEFINIR LOS TIPOS
-- CLAUSULA Y LITERAL QUE RESOLVISTE EN EL LABORATORIO.
data Literal = LI Atomo
   |NE Atomo deriving (Eq)

data Clausula = CL Literal
   |Ope Clausula Bin Clausula deriving (Eq)

data Bin = Dis deriving(Eq)

instance Show Bin where
   show (Dis) = " v "

instance Show Literal where
   show (LI a) = show a
   show (NE a) = "¬("++show a++")"

instance Show Clausula where
   show (CL (LI l)) = show l
   show (CL (NE l)) = "¬("++show l++")"
   show (Ope c1 o c2) = show c1 ++ show o ++ show c2

-- Hace parte de la familia Show al tipo Atomo.
instance Show Atomo where
   show (Var v) = show v
   show (Cte b) = show b

-- Hace parte de la familia Show al tipo OpBin.
instance Show OpBin where
   show (Conj) = " ∧ "
   show (Disy) = " ∨ "
   show (Impl) = " => "
   show (Syss) = " <=> "

-- Hace parte de la familia Show al tipo Prop.
instance Show Prop where
   show (FA a) = show a
   show (Neg p) = "¬(" ++ show p ++ ")"
   show (Op p o q) = "(" ++ show p ++ show o ++ show q ++ ")"


-- Función que realiza la sustitución simultánea dada una lista con las 
-- sustituciones.
sustSimult :: Prop -> [(VarP, Prop)] -> Prop
sustSimult (FA (Cte c)) l = (FA (Cte c))
sustSimult (FA (Var v)) l = buscaSustitucion v l
sustSimult (Neg x) l = Neg (sustSimult x l)
sustSimult (Op p o q) l = Op (sustSimult p l) o (sustSimult q l)  


buscaSustitucion::VarP ->[(VarP, Prop)]-> Prop
buscaSustitucion x [] = (FA (Var x))
buscaSustitucion x ((v,p):ys) 
  | x == v = p 
  | otherwise = buscaSustitucion x ys 

-- Función que regresa el valor de interpretación aplicada a una función en los
-- estados recibidos como parámetros.
interpreta :: Prop -> [Estado] -> Booleano
interpreta (FA (Var c)) [] = error "No se encontŕo el estado de la variable" 
interpreta (FA (Var c)) e = interpreta (buscaInterpretacion (FA (Var c)) e) e
interpreta (FA (Cte V)) _ = V
interpreta (FA (Cte F)) _ = F
interpreta (Neg (FA (Cte F))) e = V
interpreta (Neg (FA (Cte V))) e = F
interpreta (Neg x) e = interpreta (Neg (FA (Cte (interpreta x e)))) e
interpreta (Op p o q) e
  | o == Conj && ((interpreta p e) == (interpreta q e) && (interpreta p e) == V) =  V
  | o == Conj = F
  | o == Disy && ((interpreta p e) == (interpreta q e) && (interpreta p e) ==  F) = F
  | o == Disy = V
  | o == Impl && (interpreta p e) == V && (interpreta q e) == F = F
  | o == Impl = V
  | o == Syss && (interpreta p e) == (interpreta q e) = V
  | o == Syss = F


buscaInterpretacion :: Prop -> [Estado] -> Prop
buscaInterpretacion (FA (Var c)) [] = error "No se encontŕo el estado de la variable"
buscaInterpretacion (FA (Var v)) ((var, b):ys)
  | v == var = (FA (Cte b)) 
  | otherwise = buscaInterpretacion (FA (Var v)) ys

-- Función que dada una fórmula, elimina: dobles negaciones, disyunciones o 
-- conjunciones de la misma variable y disyunciones con constantes.

-- Función que regresa la forma normal negativa de una expresión
formaNN :: Prop -> Prop
formaNN (FA (Var v)) = (FA (Var v))
formaNN (FA (Cte c)) = (FA (Cte c))
formaNN x = simplifica (apNegacion (elimCon x))


--formaNN (Neg x) = apNegacion x
elimCon:: Prop -> Prop
elimCon (FA (Var v)) = (FA (Var v))
elimCon (FA (Cte c)) = (FA (Cte c))
elimCon (Neg x) = Neg (elimCon x)
elimCon (Op p o q)
  | o == Conj = (Op (elimCon p) Conj (elimCon q))
  | o == Disy = (Op (elimCon p) Disy (elimCon q))
  | o == Impl = (Op (Neg (elimCon p)) Disy (elimCon q))
  | o == Syss = (Op (elimCon(Op p Impl q)) Conj (elimCon(Op q Impl p)))
  | otherwise = (Op p o q)

--consta
apNegacion:: Prop -> Prop 
apNegacion (FA (Var v)) = (FA (Var v))
apNegacion (FA (Cte c)) = (FA (Cte c))
apNegacion (Neg (FA (Var v))) = (Neg (FA (Var v)))
apNegacion (Neg (FA (Cte c))) = (Neg (FA (Cte c)))
apNegacion (Neg (Neg x)) = apNegacion x
apNegacion (Op p o q) = (Op (apNegacion p) o (apNegacion q))
apNegacion (Neg (Op p o q))
  | o == Disy = (Op (apNegacion (Neg p)) Conj (apNegacion (Neg q)))
  | o == Conj = (Op (apNegacion (Neg p)) Disy (apNegacion (Neg q)))
  | otherwise = (Op p o q)

-- Función que regresa la forma normal conjuntiva de una expresión
formaNC :: Prop -> Prop
formaNC x = distr (formaNN x)

--Funcion que aplica distributividad a una formula proposicional
distr :: Prop -> Prop
distr (FA (Var v)) = (FA (Var v))
distr (FA (Cte c)) = (FA (Cte c))
distr (Neg x) = Neg (distr x)
distr (Op p Disy (Op q Conj r))= (Op (formaNC(Op p Disy q)) Conj (formaNC(Op p Disy r)))  
distr (Op (Op q Conj r) Disy p)= (Op (formaNC(Op p Disy q)) Conj (formaNC(Op p Disy r)))  
distr x = x

-- Función que verifica si una fórmula es tautología
esTautologia :: Prop -> Booleano
esTautologia f = auxTautologia  f (estados  f)

auxTautologia :: Prop -> [[Estado]] -> Booleano
auxTautologia p [] = V
auxTautologia p (x:xs)
  | interpreta p x == F = F
  | otherwise = auxTautologia p xs

-- Función que decide si una fórmula es satisfacible
esSatisfacible :: Prop -> Booleano
esSatisfacible (FA (Cte b)) = b
esSatisfacible p = auxSatisfacible p (estados p)

-- Función que decide si todos los estados son modelos
auxSatisfacible :: Prop -> [[Estado]] -> Booleano
auxSatisfacible p [] = F
auxSatisfacible p (x:xs)
  | interpreta p x == V = V
  | otherwise = auxSatisfacible p xs

--Función que regresa los estados posibles de una proposicion
estados :: Prop -> [[Estado]]
estados p = tablaVerdad ( unicaVez (vars p)) 

--Funcion que regresa la tabla de verdad de a cuerdo a las variables 
tablaVerdad :: [Prop] -> [[(Estado)]]
tablaVerdad [] = [[]]
tablaVerdad ((FA (Cte x)):xs) = tablaVerdad xs
tablaVerdad ((FA (Var x)):xs) = (map ((x,V):) ts) ++ (map ((x,F):) ts) 
  where ts = tablaVerdad xs

-- Función que obtiene las cláusulas de una fórmula
clausulas :: Prop -> [Prop]
clausulas x = creaClausulas (formaNC x)

creaClausulas:: Prop -> [Prop]
creaClausulas (Op p Disy q) = [(Op p Disy q)]
creaClausulas (Op (Op p Conj q) Conj r) =  [p]++ creaClausulas q ++ creaClausulas r
creaClausulas (Op p Conj q) =  [p] ++ creaClausulas q
creaClausulas x = [x]


--Ejercicios Sesion 3 de laboratorio (nos sirven para alguanas funciones)

--1 a)
conjuncion:: Booleano-> Booleano-> Booleano
conjuncion V V = V
conjuncion _ _ = F

--1 b)
implicacion :: Booleano -> Booleano -> Booleano
implicacion V F = F
implicacion _ _ = V

--1 c)
equivalencia :: Booleano -> Booleano -> Booleano
equivalencia V V = V
equivalencia F F = V
equivalencia _ _ = F

--2 a)
con:: Prop -> Int
con (FA x) =0
con (Neg x) = 1 + con x
con (Op p o q) = 1 + con p + con q

--2 b)
vars :: Prop -> [Prop]
vars (FA x) = [(FA x)] 
vars (Neg p) = vars p
vars (Op p o q) = vars p ++ vars q

--2 c)
atom :: Prop -> Int
atom (FA _) = 1
atom (Neg p) = atom p
atom (Op p o q) = atom p + atom q


-----Función auxiliar que utilizaremos para eliminar variables repetidas en
-----la tabla de verdad.
unicaVez :: [Prop] -> [Prop]
unicaVez [] = []
unicaVez (x:xs) = if elem x xs then  unicaVez xs else x:unicaVez xs

--------------------------------------------------------------------------------
--                MODIFICACIONES A LA PRÁCTICA 3                              --
-- Las funciones anteriores debieron implementarse en la práctica 2 y pueden  --
-- servir como auxiliares para esta práctica.                                 --
--------------------------------------------------------------------------------

-- Función que recibe una cláusula y la pasa a formula proprosicional
clausulaToProp :: Clausula -> Prop
clausulaToProp (CL (LI l)) = (FA l) 
clausulaToProp (CL (NE (l))) = (Neg (FA l))
clausulaToProp (Ope (p) Dis (q)) = (Op (clausulaToProp(p)) Disy (clausulaToProp(q)))

-- Funcion que regresa una lista de todas las variables de una Proposición
varP :: Prop -> [VarP]
varP p = nub (var p) 

var :: Prop -> [VarP]
var (FA (Var x)) =  [x]
var (Neg (FA (Var x))) = [x] 
var (Op (FA (Var x)) o (FA (Var y))) = [x] ++ [y]
var (Op p o q) = (var p ++ var q)

--Función que elimina elementos repetidos en una lista.
nub :: (Eq a) => [a] -> [a]
nub = nubBy (==)

nubBy :: (a -> a -> Bool) -> [a] -> [a]
nubBy eq [] = []
nubBy eq (x:xs) = x : nubBy eq (filter (\y -> not (eq x y)) xs)

--funcion que compara elementos de listas.
contains [] y = True
contains (x:xs) y = elem x y && contains xs y

equals x y = contains x y && contains y x

-- Función que toma dos fórmulas proposicionales e indica si son equivalentes.
equivalentes :: Prop -> Prop -> Booleano
equivalentes p q
  | not (equals (varP(p)) (varP(q))) = F
  | aplicaInterpretaciones p q (estados p) (estados q) == V = V
  |otherwise = F

aplicaInterpretaciones :: Prop -> Prop -> [[Estado]] -> [[Estado]] -> Booleano
aplicaInterpretaciones p q x y
  | inter p x == inter q y = V
  |otherwise = F

inter :: Prop -> [[Estado]] -> [Booleano]
inter p x = map (interpreta p) x 

-- Función que dada una fórmula, la simplifica. Esta es una versión mejorada a
-- la versión de la práctica 2.
simplifica :: Prop -> Prop
simplifica (FA (Cte c)) = (FA (Cte c))
simplifica (FA (Var v)) = (FA (Var v))
simplifica (Neg (Neg x)) = simplifica x
simplifica (Neg x) = Neg (simplifica x)
simplifica (Op p o q) 
  | p == q && (o == Conj || o == Disy) = p
  | p == (FA (Cte V)) && o == Conj = q 
  | q == (FA (Cte V)) && o == Conj = p 
  | p == (FA (Cte F)) && o == Disy = q
  | q == (FA (Cte F)) && o == Disy = p
  | p == (FA (Cte F)) && o == Conj = (FA (Cte F))
  | q == (FA (Cte F)) && o == Conj = (FA (Cte F))
  | p == (FA (Cte V)) && o == Disy = (FA (Cte V))
  | q == (FA (Cte V)) && o == Disy = (FA (Cte V))
  | otherwise = (Op p o q)

-- Función que toma dos cláusulas y una literal como parámetro y regresa su 
-- resolución binaria.
-- resBin :: Clausula -> Clausula -> Literal -> Clausula
-- resBin c1 l l = c1
-- resBin c1 c2 l 
--   | 

--PTO. EXTRA

--Función que toma una lista de cláusulas y decide si el conjunto es 
--satisfacible.

satisfacible :: [Clausula] -> Booleano
satisfacible [] = V
satisfacible l = esSatisfacible (uneClausulas l)

--Funcion auxiliar que crea una conjuncion de claúsulas.
uneClausulas :: [Clausula] -> Prop
uneClausulas [(CL (LI l))] = (FA l)
uneClausulas [(CL (NE (l)))] = (Neg (FA l))
uneClausulas (x:xs) = (Op (clausulaToProp x) Conj (uneClausulas xs))
