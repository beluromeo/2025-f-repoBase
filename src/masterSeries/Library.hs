module Library where
import PdePreludat
import Data.Char (toUpper)

-- import qualified Library as head

doble :: Number -> Number
doble numero = numero + numero

type Palabra = String
type Verso = String
type Estrofa = [Verso]
type Artista = String -- Solamente interesa el nombre

esVocal :: Char -> Bool
esVocal = flip elem "aeiou"

tieneTilde :: Char -> Bool
tieneTilde = flip elem "áéíóú"

cumplen :: (a -> b) -> (b -> b -> Bool) -> a -> a -> Bool
cumplen f comp v1 v2 = comp (f v1) (f v2)

------------------------------------------------------------------

type Rima = Palabra -> Palabra -> Bool

rima :: Rima
rima unaRima otraRrima = (rimaAsonante unaRima otraRrima || rimaConsonante unaRima otraRrima) && not (palabrasIguales unaRima  otraRrima)

rimaAsonante :: Rima
rimaAsonante unaRima otraRima = ultimasLetras 2 (vocales unaRima) == ultimasLetras 2 (vocales otraRima)

vocales :: Palabra -> String
vocales = filter esVocalOTieneTilde

esVocalOTieneTilde :: Char -> Bool
esVocalOTieneTilde letra = esVocal letra || tieneTilde letra

rimaConsonante :: Rima
rimaConsonante unaRima otraRima = ultimasLetras 3 unaRima == ultimasLetras 3 otraRima 

ultimasLetras :: Number -> Palabra -> Palabra 
ultimasLetras l palabra = take l (reverse palabra)

palabrasIguales :: Rima
palabrasIguales = (==)

--  ----------------------------------------------------------------------

conjugaciones :: [Verso -> Verso -> Bool]
conjugaciones = [porRimas, porAnadiplosis]

conjugacion :: Verso -> Verso -> Bool
conjugacion unVerso otroVerso = any (\c -> c unVerso otroVerso) conjugaciones 

-- :t any :: (a->Bool) -> [a] -> Bool

porRimas :: Verso -> Verso -> Bool
porRimas unVerso otroVerso = rima (ultimaPalabra unVerso) (ultimaPalabra otroVerso)

ultimaPalabra :: String -> String
ultimaPalabra = last . words 

primeraPalabra :: String -> String
primeraPalabra = head . words

porAnadiplosis :: Verso -> Verso -> Bool
porAnadiplosis unVerso otroVerso = rima (primeraPalabra unVerso) (ultimaPalabra otroVerso)

type Patrones = Estrofa -> Estrofa
