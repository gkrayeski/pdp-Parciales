data Auto = Auto {
    color :: String,
    velocidad :: Int,
    distancia :: Int
}deriving (Show, Eq)

type Carrera = [Auto]

autoCerca :: Auto -> Auto -> Bool
autoCerca autoUno autoDos = abs (distancia autoUno - distancia autoDos) < 10

todosMenosEseAutoCarrera :: Carrera -> Auto -> Carrera
todosMenosEseAutoCarrera carrera unAuto = filter (\auto -> color auto/= color unAuto ) carrera

vaTranquilo ::  Auto -> Carrera -> Bool
vaTranquilo unAuto carrera = all (\auto -> distancia auto <distancia unAuto) (todosMenosEseAutoCarrera carrera unAuto) && not (any ( autoCerca unAuto) (todosMenosEseAutoCarrera carrera unAuto))

puestoAuto :: Carrera -> Auto -> Int
puestoAuto unaCarrera unAuto = length (filter (\auto -> distancia auto >distancia unAuto) (todosMenosEseAutoCarrera unaCarrera unAuto)) + 1

mruAuto :: Auto ->Int ->  Auto
mruAuto auto tiempo = auto {distancia = distancia auto + velocidad auto * tiempo} 

type PowerUps = Auto -> Carrera -> Carrera
 
terremoto :: PowerUps
terremoto unAuto carrera = [unAuto] ++ filter (`vaTranquilo` carrera) carrera ++ map (\auto -> auto {velocidad = max 0 (velocidad auto - 50)}) (filter ( autoCerca unAuto) (todosMenosEseAutoCarrera carrera unAuto))

miguelito :: Int -> PowerUps
miguelito unNumero unAuto carrera = unAuto : map (\auto -> auto {velocidad = max 0 (velocidad auto) - unNumero}) (filter (\auto -> puestoAuto carrera auto> puestoAuto carrera unAuto) carrera) ++ (filter (\auto -> puestoAuto carrera auto < puestoAuto carrera unAuto) carrera)

jetPack :: Int -> PowerUps
jetPack tiempo unAuto carrera = unAuto {distancia = distancia unAuto + tiempo * velocidad unAuto * 2 } : todosMenosEseAutoCarrera carrera unAuto


auto1 :: Auto 
auto1 = Auto "rojo" 150 162

auto2 :: Auto 
auto2 = Auto "verde" 92 165

auto3 :: Auto 
auto3 = Auto "naranja" 84 292

auto4 :: Auto 
auto4 = Auto "azul" 75 163

carreraFinal :: Carrera
carreraFinal = [auto1,auto2,auto3,auto4]
