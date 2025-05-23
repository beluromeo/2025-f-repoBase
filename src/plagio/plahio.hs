module Library where
import PdePreludat


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
versionCruda  = sinAcentos . soloAlfanumericos

sinAcentos :: Texto -> Texto
sinAcentos = map sacarAcento

sacarAcento :: Char -> Char
sacarAcento 'á' = 'a'
--y todo el resto
sacarAcento letra = letra

soloAlfanumericos :: String -> String
soloAlfanumericos = filter esLetraOnumero

esLetraOnumero ::  Char -> Bool
esLetraOnumero caracter = caracter `elem` caracteresValidos

caracteresValidos :: [Char]
caracteresValidos = ['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9'] ++ " "

-- 3) -----------------------------------------------------------------------------------------------------------------

--tomo la primera letra de cada palabra y dejo el resto y voy repitiendo
-- "hola " "pera " 3
-- "ola"  "era" 2
-- "la" "ra" 1
-- "a"  "a" ya esta
distanciaHamming :: String -> String -> Number
distanciaHamming [][] = 0
distanciaHamming (x:xs) (y:ys)
      | x /= y = 1 + distanciaHamming xs ys -- si son distintos la cabeza de la cola entonces +1 y la distancia entre las colas
      | otherwise = distanciaHamming xs ys

--4) -------------------------------------------------------------------------------------------------------------------

type FormaPlagio = Obra -> Obra -> Bool

formasDeteccionPlagio :: [FormaPlagio]
formasDeteccionPlagio = [copiaLiteral, empiezaIgual 3 ,  distanciaH 10]-- or

deteccionPlagio :: Obra -> Obra -> FormaPlagio -> Bool
deteccionPlagio unaObra otraObra = any (formasDeteccionPlagio otraObra unaObra)  && anioPosterior unaObra otraObra

anioPosterior :: Obra -> Obra -> Bool
anioPosterior original plagio = anioPublicacion original < anioPublicacion plagio

copiaLiteral :: FormaPlagio
copiaLiteral original plagio  = versionCruda (texto original) == versionCruda (texto plagio)

empiezaIgual :: Number -> FormaPlagio
empiezaIgual n original plagio = primerosCaracteresIguales n original plagio  && longitudMenor original plagio

primerosCaracteresIguales :: Number -> Obra -> Obra -> Bool
primerosCaracteresIguales n original plagio = primerosCaracteres n original == primerosCaracteres n plagio

primerosCaracteres :: Number -> Obra -> String
primerosCaracteres n obra = take n (texto obra)

longitudMenor :: Obra -> Obra -> Bool
longitudMenor original plagio = length (texto original) < length (texto plagio)

-- falta agregarIntro

distanciaH :: Number -> FormaPlagio
distanciaH n unaObra otraObra = distanciaHamming (texto unaObra ) (texto otraObra) <  n

-- si la ultima palabra de un texto original tiene la misma cantidad de caracteres que el tlimo de otra
nuevaFormaPlagio :: FormaPlagio
nuevaFormaPlagio = \original plagio -> (cantCaracteresUltimaPlabra . texto)  original == (cantCaracteresUltimaPlabra . texto ) plagio

cantCaracteresUltimaPlabra :: Texto -> Number
cantCaracteresUltimaPlabra = length . ultimaPalabra

ultimaPalabra :: Texto -> Texto
ultimaPalabra = last . words

data Bot = UnBot {
    formas :: [FormaPlagio],
    fabricante :: String
} deriving (Show)

bot1 :: Bot
bot1 = UnBot [copiaLiteral] "fabricanteA"

bot2 :: Bot
bot2 = UnBot [copiaLiteral, empiezaIgual 3, distanciaH 3] "fabricanteB"

deteccion :: Bot -> Obra -> Obra -> Bool
deteccion bot unaObra otraObra = any (deteccionPlagio unaObra otraObra) (formas bot)



