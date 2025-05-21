module Library where
import PdePreludat

data Mago = UnMago{
    nombre :: String,
    edad :: Number,
    salud :: Number,
    hechizos :: [Hechizo]
}

type Hechizo = Mago -> Mago

alterarVida :: Number -> Mago -> Mago
alterarVida numero mago = mago{salud = salud mago + numero}

--1a)

curar:: Number -> Hechizo
curar = alterarVida

--1b)

lanzarRayo :: Hechizo
lanzarRayo mago
        |salud mago > 10 = alterarVida (-10) mago
        |otherwise = alterarVida (sacarMitad mago) mago

sacarMitad :: Mago -> Number
sacarMitad = (/(-2)) . salud

--1c)

amnesia :: Number -> Hechizo
amnesia = sacarHechizos

sacarHechizos :: Number -> Mago -> Mago
sacarHechizos n mago = mago{hechizos = drop n (hechizos mago)}

--1d)

confundir :: Hechizo
confundir mago = primerHechizo mago mago

primerHechizo :: Mago -> Hechizo
primerHechizo = head . hechizos

-- 2) ----------------------------------------------

-- a)

poder :: Mago -> Number
poder mago = salud mago + edad mago * cantidadHechizos mago

cantidadHechizos :: Mago -> Number
cantidadHechizos = length . hechizos

-- b)

danio :: Hechizo -> Mago -> Number
danio hechizo mago = salud mago - (salud . hechizo) mago

-- c)

diferenciaDePoder :: Mago -> Mago -> Number
diferenciaDePoder unMago otroMago = abs (poder unMago - poder otroMago)


--3)

data Academia = Academia {
    magos :: [Mago],
    examenDeIngreso :: Mago -> Bool
}

--a)

estaRicewind :: Academia -> Bool
estaRicewind academia = any esRincewindSinHechizos (magos academia)

esRincewindSinHechizos :: Mago -> Bool
esRincewindSinHechizos mago = nombre mago == "Ricewind" &&  (null . hechizos) mago

--b)

todosLosViejosSonNionios :: Academia -> Bool
todosLosViejosSonNionios = all esNionio . filter esViejo . magos

esNionio :: Mago -> Bool
esNionio mago = ((>salud mago) . cantidadHechizos) mago

esViejo :: Mago -> Bool
esViejo mago = edad mago > 50

--c)

noLoPasaria :: Academia -> Mago -> Bool
noLoPasaria academia = not . examenDeIngreso academia

quienesNoLoPasarian :: Academia -> [Mago]
quienesNoLoPasarian academia = filter (noLoPasaria academia) (magos academia)

cuantosNoLoPasarian :: Academia -> Number
cuantosNoLoPasarian = length . quienesNoLoPasarian

--d)

tieneMasDe10Hechizos :: Mago -> Bool
tieneMasDe10Hechizos = (>10) . length . hechizos

sumatoriaDeEdadDeLosQueTienenMasDe10Hechizos :: Academia -> Number
sumatoriaDeEdadDeLosQueTienenMasDe10Hechizos = sum . map edad . filter tieneMasDe10Hechizos . magos


--4) ------------------------------------------------------------


maximoSegun :: (a1 -> a2) -> a1 -> [a1] -> a1
maximoSegun criterio valor = foldl1 ((mayorSegun . criterio) valor)

mayorSegun :: Ord a => (t -> a) -> t -> t -> t 
mayorSegun evaluador comparable1 comparable2
        | evaluador comparable1 >= evaluador comparable2 = comparable1
        | otherwise                                      = comparable2

mejorHechizoContra :: Mago -> Mago -> Hechizo
mejorHechizoContra atacante objetivo = maximoSegun (`danio` objetivo) (head (hechizos atacante)) (hechizos atacante)


mejorOponente :: Mago -> Academia -> Mago 
mejorOponente mago academia = maximoSegun (diferenciaDePoder mago) (head (magos academia)) (magos academia)


--5)

aplicarTodosLosHechizos :: [Hechizo] -> Mago -> Mago
aplicarTodosLosHechizos hechizos mago = foldl (flip ($)) mago hechizos

noPuedeGanarle :: Mago -> Mago -> Bool
noPuedeGanarle mago1 mago2 = salud mago1 == salud (aplicarTodosLosHechizos (hechizos mago2) mago1) 