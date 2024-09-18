data Prop = Var String | Cons Bool | Not Prop
 | And Prop Prop | Or Prop Prop
 | Impl Prop Prop | Syss Prop Prop
 deriving (Eq)

instance Show Prop where
 show (Cons True) = "Verdadero"
 show (Cons False) = "Falso"
 show (Var p) = p
 show (Not p) = "¬" ++ show p
 show (Or p q) = "(" ++ show p ++ " v " ++ show q ++ ")"
 show (And p q) = "(" ++ show p ++ " ^ " ++ show q ++ ")"
 show (Impl p q) = "(" ++ show p ++ " -> " ++ show q ++ ")"
 show (Syss p q) = "(" ++ show p ++ " <-> " ++ show q ++ ")"

p, q, r, s, t, u :: Prop
p = Var "p"
q = Var "q"
r = Var "r"
s = Var "s"
t = Var "t"
u = Var "u"

clausulaVac :: String
clausulaVac = "[]"

type Literal = Prop

type Clausula = [Literal]

-- Función para eliminar duplicados de la lista de clausulas.
noDup :: Clausula -> Clausula
noDup [] = []
noDup (x:xs) = if (elem x xs) then (noDup xs) else ([x] ++ noDup xs)


-- Función para la forma normal negativa (FNN) de una proposición.
fnn :: Prop -> Prop
fnn (Var p) = Var p
fnn (Cons c) = Cons c
fnn (Not (Not n)) = fnn n
fnn (Not q) = negar q
fnn (And r s) = And (fnn r) (fnn s)
fnn (Or t u) = Or (fnn t) (fnn u)
fnn (Impl v w) = fnn (Or (Not v) w)
fnn (Syss x y) = fnn (And (Impl x y) (Impl y x))

{--
Función para eliminar las negaciones que no forman parte
de las caracteristicas de la forma normal negativa (FNN).
--}
negar :: Prop -> Prop
negar (Var p) = Not (Var p)
negar (Cons q) = Cons (not q)
negar (Not p) = p
negar (Or r s) = And (negar r) (negar s)
negar (And t u) = Or (negar t) (negar u)
negar (Impl v w) = And v (negar w)
negar (Syss x y) = negar (And (negar (Impl x y)) (negar (Impl y x)))

-- Funcion fnc que pasa a fnn y delega a Aux
fnc :: Prop -> Prop
fnc x =
    let y = fnn x
    in fncAux y

-- Funcion auxiliar, si OR entonces delega a distribuir
fncAux :: Prop -> Prop
fncAux (Var p) = Var p
fncAux (Not (Var p)) = Not (Var p)
fncAux (And p q) = And (fncAux p) (fncAux q)  
fncAux (Or p q) = distribuir (Or (fncAux p) (fncAux q))  

-- Funcion distribuidora de OR
distribuir :: Prop -> Prop
distribuir (Or p q) 
    | esLiteral p && esLiteral q = Or p q
distribuir (Or (And p1 p2) q) = And (Or (fncAux p1) q) (Or (fncAux p2) q)
distribuir (Or p (And q1 q2)) = And (Or p (fncAux q1)) (Or p (fncAux q2))
distribuir (Or p q) = Or p q


-- Funcion para caso base, si es literal, devuelvo true
esLiteral :: Prop -> Bool
esLiteral (Var p) = True
esLiteral (Not (Var p)) = True
esLiteral _ = False


--AUX de clausulas, caso p v q v r
clausulasAux :: Prop -> Clausula
clausulasAux (Or p q) = clausulasAux p ++ clausulasAux q
clausulasAux p = [p]

{--
Funcion para fnc, no es necesario el nodup ya que confiamos en que nada se repite

Casos a pensar, como esta en fnc no hay syss ni implicaciones, ademas el Or no importa,
esto debido a que el coneectivo importante es el "^", recordamos que esta en fnc syss
la formula se ve tal que C1 ^ C2 ^ ... ^ Cn con Ci siendo una clausula. 
--}
clausulas :: Prop -> [Clausula]
clausulas (Var p) = [[Var p]]
clausulas (Not (Var p)) = [[Not (Var p)]]
clausulas (Cons True) = [[Cons True]]
clausulas (Cons False) = [[Cons False]]
clausulas (Or p q) = [clausulasAux(Or p q)]
clausulas (And p q) = clausulas p ++ clausulas q


--Funcion auxiliar que ayuda a eliminar literales repetidas
elimina :: (Eq a) => a -> [a] -> [a]
elimina _ [] = []
elimina y (x:xs) = if y == x then xs else x:(elimina y xs)

-- Funcion auxiliar que devuelve la resolucion para 2 clausulas dadas
resolucionAux :: Clausula -> Clausula -> Clausula
resolucionAux [] ys = ys
resolucionAux ((Var p):xs) ys = if (Not (Var p)) `elem` ys then xs ++ (elimina (Not (Var p)) ys) else (Var p):(resolucionAux xs ys)
resolucionAux ((Not (Var p)):xs) ys = if (Var p) `elem` ys then xs ++ (elimina (Var p) ys) else (Not (Var p)):(resolucionAux xs ys)

-- Función principal
resolucion :: Clausula -> Clausula -> Clausula
resolucion xs ys = noDup (resolucionAux xs ys)

-- Si resolucion es distinto del vacio entonces verdadero, caso contrario, falso
-- CASO DE QUE TE DEN P V Q , R V S, DEBO CONTEMPLARLO?
-- CASO YA CONTEMPLADO, PREGUNTAR AL AYUDANTE SOBRE SI ES TRUCULENTO
hayResolvente :: Clausula -> Clausula -> Bool
hayResolvente xs ys = 
    let res = resolucion xs ys
        esVacia = null res
        mismaLongitud = length res == length xs + length ys
    in not (esVacia || mismaLongitud)





