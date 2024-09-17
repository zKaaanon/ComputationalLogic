data Prop = Var String | Cons Bool | Not Prop
 | And Prop Prop | Or Prop Prop
 | Impl Prop Prop | Syss Prop Prop
 deriving (Eq)

instance Show Prop where
 show (Cons True) = "Verdadero"
 show (Cons False) = "Falso"
 show (Var p) = p
 show (Not p) = "¬" ++ show p
 show (Or p q) = "(" ++ show p ++ " ∨ " ++ show q ++ ")"
 show (And p q) = "(" ++ show p ++ " ∧ " ++ show q ++ ")"
 show (Impl p q) = "(" ++ show p ++ " → " ++ show q ++ ")"
 show (Syss p q) = "(" ++ show p ++ " ↔ " ++ show q ++ ")"

p, q, r, s, t, u :: Prop
p = Var "p"
q = Var "q"
r = Var "r"
s = Var "s"
t = Var "t"
u = Var "u"

type Estado = [String]

-- Función para obtener el conjunto de variables de una fórmula proposicional.
variables :: Prop -> Estado
variables (Var p) = [p]
variables (Cons b) = []
variables (Not p) = variables(p)
variables (And q r) = noDup(variables(q) ++ variables(r))
variables (Or s t) = noDup(variables(s) ++ variables(t))
variables (Impl u v) = noDup(variables(u) ++ variables(v))
variables (Syss w x) = noDup(variables(w) ++ variables(x))

-- Función para eliminar duplicados de la lista obtenida de variables.
noDup :: Estado -> Estado
noDup [] = []
noDup (x:xs) = if (elem x xs) then (noDup xs) else ([x] ++ noDup xs)

-- Función para el conjunto potencia de una lista de elementos dada.
conjPotencia :: [a] -> [[a]]
conjPotencia [] = [[]]
conjPotencia (x:xs) = [(x:ys) | ys <- conjPotencia xs] ++ conjPotencia xs

-- Función de interpretación para una fórmula dada una lista de variables con estado I(p)=1.
interpretacion :: Prop -> Estado -> Bool
interpretacion (Var p) b = if (elem p b) then True else False
interpretacion (Cons const) b = const
interpretacion (Not q) b = not(interpretacion q b)
interpretacion (And r s) b = (interpretacion r b) && (interpretacion s b)
interpretacion (Or t u) b = (interpretacion t b) || (interpretacion u b)
interpretacion (Impl v w) b = not(interpretacion v b) || (interpretacion w b)
interpretacion (Syss x y) b = ((interpretacion x b) && (interpretacion y b)) || (not(interpretacion x b) && not(interpretacion y b))

-- Función de estados posibles. POR DEFINIR BIEN LA FUNCIÓN.
estadosPosibles :: Prop -> [Estado]
estadosPosibles localProp = filter (\estado -> interpretacion localProp estado) (conjPotencia (variables localProp))

{--
Función para indicar si una fórmula es tautologia.
Podría usarse la función interpretación y verificar cada subconjunto
de interpretaciones (correspondiente a cada renglón de la tabla de verdad),
luego juntarlo todo con conjunciones (and/&&) y si alguna es falsa entonces
la interpretación para comprobar si es tautología evalua en falso.
--}
tautologia :: Prop -> Bool
tautologia proposicion = and [interpretacion proposicion renglonTabla | renglonTabla <- estados]
 where estados = conjPotencia(variables proposicion)

{--
Función para indicar si una fórmula es contradiccion.
Similar a la función tautologia, pero ahora evaluando sobre las disyunciones.
Si existe alguna interpretación que lleve a la proposición a ser verdadera, entonces no es
contradicción pues es satisfacible.
--}
contradiccion :: Prop -> Bool
contradiccion proposicion = if (or [interpretacion proposicion renglonTabla | renglonTabla <- estados]) then False else True
 where estados = conjPotencia(variables proposicion)

--Función para verificar si una interpretación es modelo para una fórmula proposicional.
esModelo :: Estado -> Prop -> Bool
esModelo estados proposicion = if (interpretacion proposicion estados) then True else False

-- Función para obtener los modelos de una proposición.
modelos :: Prop -> [Estado]
modelos proposicion = noDup' [if (esModelo y proposicion) then y else [] | y<-x]
 where x = conjPotencia(variables proposicion)

-- Función para eliminar duplicados de la lista obtenida de estados.
noDup' :: [Estado] -> [Estado]
noDup' [] = []
noDup' (x:xs) = if (elem x xs) then (noDup' xs) else ([x] ++ noDup' xs)

-- Función para verificar si una fórmula proposicional es válida.
esValida :: Prop -> Bool
esValida a = tautologia a

--Función para verificar si una fórmula proposicional es insatisfacible.
esInsatisfacible :: Prop -> Bool
esInsatisfacible b = contradiccion b

{--
Función para verificar si una fórmula proposicional es satisfacible.
Si la proposición es satisfacible, entonces hay alguna interpretación que lleva
a no ser contradicción.
--}
esSatisfacible :: Prop -> Bool
esSatisfacible propVerificar = not(contradiccion propVerificar)



















sustituir :: String -> Prop -> Prop -> Prop
sustituir x sub (Var p) = if x == p then sub else Var p
sustituir _ _ (Cons b) = Cons b
sustituir x sub (Not p) = Not (sustituir x sub p)
sustituir x sub (And p q) = And (sustituir x sub p) (sustituir x sub q)
sustituir x sub (Or p q) = Or (sustituir x sub p) (sustituir x sub q)
sustituir x sub (Impl p q) = Impl (sustituir x sub p) (sustituir x sub q)
sustituir x sub (Syss p q) = Syss (sustituir x sub p) (sustituir x sub q)




contar :: Prop -> Int
contar (Var p) = 1
contar (Cons b) = 0
contar (Not p) = contar(p)
contar (And q r) = contar(q) + contar(r)
contar (Or s t) = contar(s) + contar(t)
contar (Impl u v) = contar(u) + contar(v)
contar (Syss w x) = contar(w) + contar(x)




