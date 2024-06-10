data Chico = Chico {
    nombre :: String,
    edad :: Int,
    habilidades :: [String],
    deseos :: [Deseos]
}deriving (Show, Eq)

type Deseos = Chico -> Chico

--punto A 1

aprenderHabilidadesNuevas :: [String] -> Chico -> Chico
aprenderHabilidadesNuevas habilidadesNuevas unChico = unChico {habilidades = habilidadesNuevas ++ habilidades unChico} 

serGrosoEnNeedForSpeed :: Deseos
serGrosoEnNeedForSpeed unChico = unChico {habilidades = map (\unNumero -> "jugar perfecto a need for speed " ++ show unNumero) [1..100] ++ habilidades unChico }

serMayor :: Deseos
serMayor unChico = unChico {edad =18} 

-- punto A 2
cambiarEdad :: (Int -> Int -> Int) -> Int -> Deseos
cambiarEdad unaFuncion unNumero unChico= unChico {edad = unaFuncion (edad unChico) unNumero }

wanda :: Deseos
wanda unChico =  head (deseos unChico) (cambiarEdad (+) 1 unChico) 

cosmo :: Deseos
cosmo = cambiarEdad div 2

type HabilidadDeseada = Chico -> Bool
-- punto 1 B
tieneHabilidad :: String -> HabilidadDeseada
tieneHabilidad unaHabilidad unChico =  unaHabilidad `elem` habilidades unChico

esSuperMaduro :: Chico -> Bool
esSuperMaduro unChico = tieneHabilidad "manejar" unChico && edad unChico > 18

-- punto 2 B

data Chica = Chica{
    nombreChica :: String,
    condiciones :: [HabilidadDeseada]
}deriving (Show, Eq)

seleccionarPretendientes :: Chica -> [Chico] -> [Chico]
seleccionarPretendientes unaChica  = filter (\chico -> all (\condicion -> condicion chico) (condiciones unaChica))

quienConquistaA :: Chica -> [Chico] -> Chico 
quienConquistaA unaChica losPretendientes 
    | length (seleccionarPretendientes unaChica losPretendientes) > 0 = head (seleccionarPretendientes unaChica losPretendientes)  
    | otherwise = last losPretendientes

valentina :: Chica
valentina = Chica "valentina" [tieneHabilidad "cocina"]

-- ultimo punto 

deseoProhibido :: Deseos -> Chico -> Bool
deseoProhibido unDeseo unChico
    | any (=="enamorar") (take 5 (habilidades (unDeseo unChico))) = True
    | any (=="dominar el mundo") (take 5 (habilidades (unDeseo unChico))) = True
    | any (=="matar") (take 5 (habilidades (unDeseo unChico))) = True   
    | otherwise = False


infractoresDeDaRules :: [Chico] -> [String]
infractoresDeDaRules unosChicos = map nombre (filter  (\chico -> any (\d -> deseoProhibido d chico) (deseos chico)) unosChicos)
