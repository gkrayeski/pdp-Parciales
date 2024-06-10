import Text.Show.Functions()

data Animal = Animal {
    iq :: Int,
    especie :: String,
    capacidades :: [String]
} deriving Show

pinky :: Animal
pinky = Animal 101 "raton" ["boludo"]

cerebro :: Animal
cerebro = Animal 99 "elefante" ["sabe matematica"]

inteligenciaSuperior :: Int -> Animal -> Animal
inteligenciaSuperior iqNuevo animal = animal {iq = iq animal + iqNuevo}

pinkificar :: Animal -> Animal
pinkificar unAnimal = unAnimal { capacidades = []}

agregarHabilidad :: Animal -> String -> Animal
agregarHabilidad unAnimal habilidad = unAnimal {capacidades = habilidad : capacidades unAnimal}

superpoderes :: Animal -> Animal
superpoderes unAnimal 
    | especie unAnimal == "elefante" = agregarHabilidad unAnimal "no tenerle miedo a los ratones"
    | especie unAnimal == "raton" && iq unAnimal > 100 =   agregarHabilidad unAnimal "hablar"
    | otherwise = unAnimal



cerebro2 :: Animal
cerebro2 = Animal 101 "raton" ["hacer aaaaa", "hola"]

esVocal :: Char -> Bool
esVocal letra = letra `elem` "aeiou"

palabraTieneVocal :: String -> Bool
palabraTieneVocal  = any (\letra -> esVocal letra)

empiezaConHacer :: String -> Bool
empiezaConHacer palabra = take 5 palabra == "hacer" 

tieneMenosDeCuatroLetras2daPalabra :: String -> Bool
tieneMenosDeCuatroLetras2daPalabra palabra =  length palabra <= 10

hacerSonidoPinkiesco :: Animal -> Bool
hacerSonidoPinkiesco unAnimal = any (\habilidad -> palabraTieneVocal (drop 6 habilidad) && empiezaConHacer habilidad && tieneMenosDeCuatroLetras2daPalabra habilidad) (capacidades unAnimal)

antropomorfico :: Animal -> Bool
antropomorfico unAnimal = elem "hablar" (capacidades unAnimal) && iq unAnimal > 150

data Experimento = Experimento {
    criterioDeExito :: [Animal -> Bool],
    transformaciones :: [Animal -> Animal]
} deriving Show

experimentoInteligencia :: Experimento
experimentoInteligencia = Experimento [antropomorfico] [inteligenciaSuperior 60, superpoderes]

aplicarExperimento :: Animal -> [Animal->Animal] -> Animal
aplicarExperimento = foldr ($) 

experimentoExitoso :: Experimento -> Animal -> Bool
experimentoExitoso unExperimento unAnimal =  all (\criterio -> criterio (aplicarExperimento unAnimal (transformaciones unExperimento))) (criterioDeExito unExperimento)



