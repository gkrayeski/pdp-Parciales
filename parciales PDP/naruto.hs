import Language.Haskell.TH (TExp)
data Ninja = Ninja {
    nombre :: String,
    herramientas :: [Herramienta],
    jutsu :: [Jutsu],
    rango :: Int
} deriving (Show, Eq)


type Jutsu = Int

data Herramienta = Herramienta {
    nombreHerramienta:: String,
    cantidad :: Int
} deriving (Show, Eq)


bombasDeHumo :: Herramienta
bombasDeHumo = Herramienta "bomba" 90

kunais:: Herramienta
kunais = Herramienta "kunais" 15

cantidadHerramientaNinja  :: Ninja -> Int
cantidadHerramientaNinja unNinja = sum (map cantidad (herramientas unNinja))

obtenerHerramienta :: Herramienta -> Ninja -> Ninja
obtenerHerramienta herramienta unNinja
    | cantidadHerramientaNinja unNinja + cantidad herramienta < 100 = unNinja {herramientas = herramienta : herramientas unNinja}
    | otherwise = unNinja {herramientas = herramienta {cantidad = 100 - cantidadHerramientaNinja unNinja}: herramientas unNinja} 

naruto :: Ninja
naruto = Ninja "naruto" [bombasDeHumo] [1,2,3] 14
hola :: Ninja
hola = Ninja "naruto2" [bombasDeHumo] [1,2,1] 14

usarHerramienta :: Herramienta -> Ninja -> Ninja
usarHerramienta unaHerramienta unNinja= unNinja {herramientas = filter (\h -> nombreHerramienta h /= nombreHerramienta unaHerramienta) (herramientas unNinja)} 


--punto 2

data Mision = Mision {
    equipoDeNinjas :: [Ninja],
    cantidadRequerida :: Int,
    rangoRecomendable :: Int,
    enemigos :: Int,
    herramientasPremio :: [Herramienta]
}

type TipoMision = Mision -> Bool

ninjaSuperaRango :: Ninja -> Int -> Bool
ninjaSuperaRango unNinja rangoMision = rango unNinja > rangoMision 

esDesafiante :: TipoMision
esDesafiante mision = all (\ninja -> ninjaSuperaRango ninja (rangoRecomendable mision)) (equipoDeNinjas mision) &&  enemigos mision > 3


cantidadDePremios :: Mision  ->String -> Int-> Bool
cantidadDePremios unaMision tipoHerramienta cantidades = sum (map cantidad (filter((==tipoHerramienta). nombreHerramienta) (herramientasPremio unaMision))) > cantidades 

esCopada :: TipoMision
esCopada unaMision = (cantidadDePremios unaMision "bomba de humo" 3 && cantidadDePremios unaMision "shurikens" 5)|| cantidadDePremios unaMision "kunais" 14

esFactible :: TipoMision
esFactible unaMision = not (esDesafiante unaMision) && sum (map cantidadHerramientaNinja (equipoDeNinjas unaMision)) > 500 || length (equipoDeNinjas unaMision)>= cantidadRequerida unaMision


quitarRangoANinja :: Ninja -> Ninja
quitarRangoANinja unNinja = unNinja { rango= max 0(rango unNinja - 2)}

type MisionTerminada = Mision -> [Ninja]

fallarMision :: MisionTerminada
fallarMision unaMision = map quitarRangoANinja (filter (\ninja -> rango ninja > rangoRecomendable unaMision) (equipoDeNinjas unaMision))

misionhola :: Mision
misionhola = Mision [naruto, hola] 1  1 5 [kunais]



