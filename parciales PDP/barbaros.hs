

{- Consideraciones: 
Escribir el tipo de todas las funciones principales
Emplear sinónimos de tipo cuando sea posible.
No se permite usar recursividad salvo que se indique lo contrario
Definir las funciones en estilo point-free cuando sea posible

Punto 1

Se sabe que los bárbaros tienen nombre, fuerza, habilidades y objetos, que los ayudarán más adelante en su lucha contra el mal. Por ejemplo: 

dave = Barbaro "Dave" 100 ["tejer","escribirPoesia"] [ardilla, varitasDefectuosas]

Se pide definir los siguientes objetos y definir algunos bárbaros de ejemplo
-}
{- 
    1 - Las espadas aumentan la fuerza de los bárbaros en 2 unidades por cada kilogramo de peso. TUKI
    2 - Los amuletosMisticos puerco-marranos otorgan una habilidad dada a un bárbaro. TUKI
    3 - Las varitasDefectuosas, añaden la habilidad de hacer magia, pero desaparecen todos los demás objetos del bárbaro. TUKI
    4 - Una ardilla, que no hace nada. TUKI
    5 - Una cuerda, que combina dos objetos distintos,obteniendo uno que realiza las transformaciones de los otros dos. 
-}


data Barbaro = Barbaro {
    nombre :: String,
    fuerza :: Int,
    habilidades :: [String],
    objetos :: [Objeto]
}
type Objeto = Barbaro -> Barbaro
type PesoEspada = Int
type Habilidad = String

espada :: PesoEspada -> Objeto
espada peso unBarbaro = unBarbaro {fuerza = fuerza unBarbaro + peso * 2, objetos = espada peso: objetos unBarbaro }

amuletosMisticos :: Habilidad -> Objeto
amuletosMisticos habilidad unBarbaro = unBarbaro {habilidades = habilidad:  habilidades unBarbaro, objetos = amuletosMisticos habilidad: objetos unBarbaro}

varitasDefectuosas :: Objeto
varitasDefectuosas unBarbaro = unBarbaro {habilidades = "magia" : habilidades unBarbaro , objetos = [varitasDefectuosas]}

unaArdilla :: Objeto
unaArdilla unBarbaro = unBarbaro { objetos= unaArdilla: objetos unBarbaro}

unaCuerda :: Objeto -> Objeto -> Objeto
unaCuerda objeto1 objeto2 unBarbaro = (objeto2 . objeto1) (unBarbaro {objetos = unaCuerda objeto1 objeto2:objetos unBarbaro})

barbaro1 :: Barbaro
barbaro1 = Barbaro "barbaro 1" 10 ["comer"] [unaArdilla]

-- punto 2

concatenarHabilidades :: Barbaro -> String
concatenarHabilidades barbaro = map toUpper (concat (habilidades barbaro))

megafono :: Objeto
megafono unBarbaro = unBarbaro {habilidades= [concatenarHabilidades unBarbaro], objetos = megafono : objetos unBarbaro}


megafonoBarbarico :: Objeto -> Objeto -> Objeto -> Objeto
megafonoBarbarico unaCuerda unaArdilla megafono  =  megafono.unaArdilla.unaCuerda

-- punto 3

type Sobrevivir = (Barbaro -> Bool)

invasionDeSuciosDuendes :: Sobrevivir
invasionDeSuciosDuendes barbaro = elem "Escribir Poesía Atroz" (habilidades barbaro)

cremalleraDelTiempo :: Sobrevivir
cremalleraDelTiempo barbaro = nombre barbaro == "Faffy" || nombre barbaro == "Astro"

esVocal :: Char -> Bool
esVocal c = toLower c `elem` "aeiou"

cantidadVocales :: String -> Int
cantidadVocales  = length . filter esVocal

listaConVocales :: [String] -> Int
listaConVocales  = sum . map cantidadVocales

empiezaConMayus :: String -> Bool
empiezaConMayus = isUpper . head

listaConMayusculas :: [String] -> Bool
listaConMayusculas = all empiezaConMayus


ritualDeFechorias :: Sobrevivir
ritualDeFechorias barbaro
    | any (=="robar") (habilidades barbaro) && fuerza barbaro > 80 = True
    | listaConVocales (habilidades barbaro) > 3 && listaConMayusculas (habilidades barbaro)  = True
    | otherwise = False

sobrevivir :: [Barbaro] -> Sobrevivir -> [Barbaro]
sobrevivir listaBarbaros prueba = filter prueba listaBarbaros








