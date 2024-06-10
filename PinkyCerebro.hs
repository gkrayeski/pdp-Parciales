import Text.Show.Functions()

{-Punto 1-}
data Animal = UnAnimal {
    iq           :: Int,
    especie      :: String,
    capacidades  :: [String]
} deriving Show

pinky :: Animal 
pinky = UnAnimal {
    iq          = 60,
    especie     = "raton",
    capacidades = ["correr", "nadar", "hablar","hacer narf", "hacer asdf", "hacer carl"]
}

cerebro :: Animal
cerebro = UnAnimal {
    iq          = 200,
    especie     = "raton",
    capacidades = ["pensar","construir"]
}

pepito :: Animal
pepito = UnAnimal {
    iq          = 17,
    especie     = "raton",
    capacidades = ["destruenglonir el mundo", "hacer planes desalmados"]
}
{-Punto 1-}

type Transformar = Animal -> Animal

cambiarIq :: (Int -> Int) -> Transformar
cambiarIq fn animal = animal {iq = fn . iq $ animal}

cambiarCapacidades :: ([String] -> [String]) -> Transformar
cambiarCapacidades fn animal = animal {capacidades = fn . capacidades $ animal}

{-Punto 2-}
inteligenciaSuperior :: Int -> Transformar
inteligenciaSuperior n animal = cambiarIq (+n) animal

pinkificar :: Transformar
pinkificar animal = cambiarCapacidades quitarTodasLasHabilidades animal

quitarTodasLasHabilidades :: [String] -> [String]
quitarTodasLasHabilidades capacidades = []

superpoderes :: Transformar
superpoderes animal 
    | especie animal == "elefante"                  = cambiarCapacidades (++["No tenerle miedo a los ratones"]) animal
    | especie animal == "raton" && iq animal > 100  = cambiarCapacidades (++["Hablar"]) animal
    | otherwise                                     = animal
{-Punto 2-}

{-Punto 3-}
type Criterio = Animal -> Bool

antropomorfico :: Criterio
antropomorfico animal = tieneHabilidadHablar animal && iq animal > 60

tieneHabilidadHablar :: Criterio
tieneHabilidadHablar animal = "hablar" `elem` capacidades animal

tieneVocal :: Char -> Bool
tieneVocal caracter = caracter `elem` "aeiouAEIOU"

tieneCuatroLetrasOMenos :: String -> Bool
tieneCuatroLetrasOMenos palabra = length palabra <= 4

esPalabraPinkinesca :: String -> Bool
esPalabraPinkinesca palabra = tieneCuatroLetrasOMenos palabra && any tieneVocal palabra

pinkinesco :: String -> Bool
pinkinesco palabra = take 6 palabra == "hacer " && esPalabraPinkinesca (drop 6 palabra)

filtrarPinkinesco :: [String] -> [String]
filtrarPinkinesco capacidades = filter pinkinesco capacidades

noTanCuerdo :: Criterio
noTanCuerdo animal = length (filtrarPinkinesco $ capacidades animal) > 2
{-Punto 3-}

{-Punto 4-}
type Experimento = ([Transformar] , Criterio)

transformacionesParaPepito :: [Transformar]
transformacionesParaPepito = [pinkificar, inteligenciaSuperior 10, superpoderes]

criterioDeExitoParaPepito :: Criterio
criterioDeExitoParaPepito = antropomorfico

experimentoPepito :: Experimento 
experimentoPepito = (transformacionesParaPepito, criterioDeExitoParaPepito)

ejecutarExperimento :: Experimento -> Animal -> Animal
ejecutarExperimento (transformaciones,criterio) animal = foldr ($) animal transformaciones

experimentoExitoso :: Experimento -> Animal -> Bool
experimentoExitoso (transformaciones,criterio) animal = criterio (ejecutarExperimento (transformaciones,criterio) animal)
{-Punto 4-}

{-Punto 5-}
listaDeAnimales :: [Animal] -> [String] -> Experimento -> [Animal]
listaDeAnimales animales capacidades experimento = ejecutarExperimentoEnAnimales animales experimento

ejecutarExperimentoEnAnimales :: [Animal] -> Experimento -> [Animal]
ejecutarExperimentoEnAnimales animales experimento = map (ejecutarExperimento experimento) animales
