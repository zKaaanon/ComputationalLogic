{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use foldr" #-}
module Proyecto1 where
import Text.Read (Lexeme(String))

{- En la primera parte nos pide realizar un tipo de dato Shape que tome como posibles valores: circulos, cuadrados, rectangulos y trapecios -}

data Shape = Circle Float | Square Float | Rectangle Float Float | Trapeze Float Float Float deriving (Show, Eq)

{- Ahora, necesitamos una funcion tal que reciba una figura y calcule su area-}

area :: Shape -> Float
area (Circle r) = pi * (r ** 2)
area (Square l) = l**2
area (Rectangle x1 x2) = x1 * x2
area (Trapeze major minor height) = ((major + minor) / 2) * height

{- Posteriormente, necesitamos una funcion tal que reciba una figura y calcule su perimeter-}

perimeter :: Shape -> Float
perimeter(Circle r) = pi * 2 * r
perimeter(Square l) = l*4
perimeter(Rectangle x1 x2) = (2 * x1) + (2 * x2)
perimeter(Trapeze major minor height) = (2 * height) + major + minor

{-Por ultimo, falta la comparacion de figuras, tomando en cuenta su area como parametros-}

instance Ord Shape where
    s1 <= s2 = area s1 <= area s2

{- Segunda parte, tenemos que hacer un tipo de dato llamado Point, el cual represente el plano cartesiano-}

data Point = Point {cX :: Float, cY :: Float} deriving (Show)

{- Asi, primero haremos la funcion que compare dos puntos entre si-}

distance :: Point -> Point -> Float
distance p1 p2 = sqrt( (cX p2 - cX p1)**2 + (cY p2 - cY p1)**2)

{- Por ultimo, compararemos la distancia de un punto al origen-}

from0 :: Point -> Float
origin = (Point {cX = 0, cY = 0})
from0 p = distance p origin

{- Haskelliums -}

data Haskellium = Haskellium { name :: String,
 lastName1 :: String,
 lastName2 :: String,
 location :: Point,
 houseShape :: Shape
 } deriving (Show)

son :: Haskellium -> Haskellium -> String -> Haskellium
son h1 h2 newName = (Haskellium{
 name = newName,
 lastName1 = lastName1 h1,
 lastName2 = lastName1 h2,
 location = location h1,
 houseShape = houseShape h1
})

houseCost :: Haskellium -> Float
houseCost h = 0

timeToWork :: Haskellium -> Float
timeToWork hask
 | from0 (location hask) <= 300 = from0 (location hask) / 30
 | from0 (location hask) > 300 = from0 (location hask) / 70

{- Finalmente, la parte de los algoritmos-}

{- Algoritmo isPal -}

-- Funcion auxiliar para calcular la reversa de una cadena
rev :: String -> String
rev "" = ""                 
rev (x:xs) = rev xs ++ [x]  

isPal :: String -> Bool
isPal s = s == rev s

{- Algoritmo concat -}

concat' :: [[a]] -> [a]
concat' [] = []
concat' (x:xs) = x ++ concat' xs

{- Algoritmo de Pascal-}

-- Funcion auxiliar para calcular el coeficiente binomial
binomial :: Int -> Int -> Int
binomial n k = product [1..n] `div` (product [1..k] * product [1..(n - k)])

pascalN :: Int -> [Int]
pascalN n = [binomial n k | k <- [0..n]]

{- Algoritmo reversaFr-}

reversaFr :: [a] -> [a]
reversaFr = foldr (\x acc -> acc ++ [x]) []







