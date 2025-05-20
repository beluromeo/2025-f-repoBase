module Library where
import PdePreludat
import System.Posix.Internals (fileType)

doble :: Number -> Number
doble numero = numero + numero


type Texto = String


data Obra = UnaObra {
    texto :: String,
    anioPublicacion :: Number
} deriving (Show, Eq)

data Autor = UnAutor {
    nombre :: String,
    obras :: [Obra]
} deriving (Show, Eq)

obraA :: Obra
obraA = UnaObra "Habia una vez" 1997

obraB :: Obra
obraB = UnaObra "!habia una vez" 1998

obraC :: Obra
obraC = UnaObra "mirtha, susana y moria" 2010

autor1 :: Autor
autor1 = UnAutor "belen" [obraA, obraB]

-- 2) ----------------------------------------------------------------------------------------------------------------

versionCruda :: Texto -> Texto 
versionCruda texto = sinAcentos . soloAlfanumericos

sinAcentos :: Texto -> Texto
sinAcentos = map sacarAcento

sacarAcento :: Char -> Char
sacarAcento 'á' = 'a'
--y todo el resto
sacarAcento letra = letra

soloAlfanumericos :: String -> String
soloAlfanumericos = filter elem caracter ['a'..'z'] ['A'..'Z'] [0..9]

-- 3) -----------------------------------------------------------------------------------------------------------------

--tomo la primera letra de cada palabra y dejo el resto y voy repitiendo
-- "hola " "pera " 3
-- "ola"  "era" 2
-- "la" "ra" 1
-- "a"  "a" ya esta
distanciaHamming :: String -> String -> Number
distanciaHamming [][] 
distanciaHamming (x:xs) (y:ys) 
      | x\= y = 1 + distanciaHamming xs ys -- si son distintos la cabeza de la cola entonces +1 y la distancia entre las colas
      | otherwise = distanciaHamming xs ys  

--4) -------------------------------------------------------------------------------------------------------------------

formasDeteccionPlagio = [copiaLiteral, emiezaIgual, agregaronIntro, distanciaH]

type FormaPlagio = Obra -> Obra -> Bool

deteccionPlagio :: FormaPlagio -> Bool
deteccionPlagio unaObra otraObra = any (\p -> unaObra otraObra) formasDeteccionPlagio && anioPosterior

anioPosterior :: Obra -> Obra -> Bool
anioPosterior original plagio = anioPublicacion original < anioPublicacion plagio

copiaLiteral :: FormaPlagio
copiaLiteral = (==) versionCruda

empiezaIgual :: FormaPlagio 
empiezaIgual = primerosCaracteres 3 && longitudMenor

primerosCaracteresIguales :: Number -> Obra -> Obra -> Bool
primerosCaracteres n original plagio = primerosCaracteres n original == primerosCaracteres n plagio

primerosCaracteres :: Number -> Obra -> String
primerosCaracteres n = take n (texto obra)

longitudMenor :: Obra -> Obra -> Bool
longitudMenor original plagio = length (texto original) < length (texto plagio)







