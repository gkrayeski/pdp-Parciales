import Text.Show.Functions()

data Participante = Participante {
    nombre :: String,
    truquitos :: [Modificacion],
    platoEspecial :: Plato
} deriving Show

data Ingrediente = Ingrediente {
    tipo :: String,
    peso :: Int
} deriving (Show, Eq)

data Plato = Plato {
    dificultad :: Int,
    conjuntoComponente :: [Ingrediente]
} deriving (Show, Eq)

type TipoDeTruco = Int -> Plato -> Plato
type Modificacion = Plato -> Plato

agregarIngrediente :: Int -> String  -> Plato -> Plato
agregarIngrediente gramo tipoDeIngrediente  unPlato = unPlato {conjuntoComponente = Ingrediente {tipo = tipoDeIngrediente , peso = gramo } : conjuntoComponente unPlato}
 
endulzar :: TipoDeTruco
endulzar gramos = agregarIngrediente gramos "azucar"

salar :: TipoDeTruco
salar gramos = agregarIngrediente gramos "sal"

darSabor :: Int -> TipoDeTruco
darSabor gramosSal gramosAzucar unPlato = agregarIngrediente gramosAzucar "azucar" (agregarIngrediente gramosSal "sal" unPlato)

aumentarPlato ::  String -> Int -> Ingrediente -> Ingrediente
aumentarPlato nombre peso unIngrediente =  unIngrediente {tipo =nombre, peso = peso * 2}

duplicarPorcion :: Modificacion
duplicarPorcion unPlato = unPlato {conjuntoComponente = map (\comp -> aumentarPlato (tipo comp) (peso comp) comp) (conjuntoComponente unPlato) }

simplificarPlato :: Modificacion
simplificarPlato unPlato = unPlato {dificultad = 5, conjuntoComponente = filter (\ing -> peso ing < 10 ) (conjuntoComponente unPlato)}

testearSimplicidad :: Modificacion
testearSimplicidad unPlato 
    | ( length (conjuntoComponente unPlato) > 5 ) && ( dificultad unPlato > 7 ) = simplificarPlato unPlato
    | otherwise = unPlato 

comprobarTipoDePlato :: Plato -> String -> Bool
comprobarTipoDePlato unPlato contenido = any ((== contenido) . (\ingre -> tipo ingre)) (conjuntoComponente unPlato)

esVegano :: Plato -> Bool
esVegano unPlato 
    | comprobarTipoDePlato unPlato "carne" = True 
    | comprobarTipoDePlato unPlato  "lacteos" = True
    | comprobarTipoDePlato unPlato "huevo" = True
    |otherwise = False


masa :: Ingrediente
masa = Ingrediente "harina" 15

platoComplejo :: Plato
platoComplejo = Plato 6 [masa]

pepe :: Participante
pepe = Participante "Pepe Roniccio" [salar 2, endulzar 5, testearSimplicidad, duplicarPorcion] platoComplejo

cocinar :: Plato -> Participante -> Plato
cocinar unPlato unPrincipiante = foldr ($) unPlato (truquitos unPrincipiante)

sumaDePesos :: Plato -> Int
sumaDePesos unPlato = sum ( map peso (conjuntoComponente unPlato))

esMejorQue :: Plato -> Plato -> Bool
esMejorQue platoUno platoDos 
    | dificultad platoUno > dificultad platoDos && sumaDePesos platoUno < sumaDePesos platoDos = True
    | otherwise = False


platinum :: Plato
platinum = Plato 10 ingredientesInfinitos


ingredientesInfinitos :: [Ingrediente]
ingredientesInfinitos =  map (\unNumero -> Ingrediente{tipo = "ingrediente " ++ show unNumero , peso = unNumero} ) [1..]


type Tupla = (Int, String)
