
-- //////////////////////////////////////////////////////////////////////////////////////
{-
1 . TIPOS ENUMERADOS
a) Implementar el tipo Carrera como esta definido arriba.
-}

data  Carrera = Matematica | Fisica | Computacion | Astronomia deriving (Show, Eq)

{-
    EJEMPLOS DE EJECUCION
    ghci> let carrera1 = Astronomia
    ghci> let carrera2 = Fisica
    ghci> carrera1
    Astronomia
    ghci> carrera2
    Fisica
    ghci> carrera1 == carrera2
    False
    ghci> carrera1 == Astronomia
    True
    ghci> carrera2 /= Computacion
    True
    ghci> carrera2 == Computacion
    False
-}


-- //////////////////////////////////////////////////////////////////////////////////////
{-
1 . TIPOS ENUMERADOS
b) Definir la siguiente funcion, usando pattern matching : titulo :: Carrera -> String
que devuelve el nombre completo de la carrera en forma de string. 
Por ejemplo, para el constructor Matematica, debe devolver ”Licenciatura en Matematica”.
-}

mensaje :: Carrera -> String
mensaje Matematica  = "Licenciatura en Matematica"
mensaje Fisica      = "Licenciatura en Fisica"
mensaje Computacion = "Licenciatura en Ciencias de la Computacion"
mensaje Astronomia  = "Licenciatura en Astronomia"


{-
    EJEMPLOS DE EJECUCION
    ghci> mensaje Matematica
    "Licenciatura en Matematica"
    ghci> mensaje Astronomia
    "Licenciatura en Astronomia"
    ghci> mensaje Computacion
    "Licenciatura en Ciencias de la Computacion"
-}


-- //////////////////////////////////////////////////////////////////////////////////////
{-
1 . TIPOS ENUMERADOS
c) Definir el tipo NotaBasica con constructores Do, Re, Mi, Fa, Sol, La y Si
-}

--data NotaBasica = Do | Re | Mi | Fa | Sol | La | Si deriving (Show)

{-
    EJEMPLOS DE EJECUCION
    ghci> Do
    Do
    ghci> Re
    Re
-}


-- //////////////////////////////////////////////////////////////////////////////////////
{-
1 . TIPOS ENUMERADOS
d) El sistema de notacion musical anglosajon, tambien conocido como notacion o cifrado
americano, relaciona las notas basicas con letras de la A a la G. Este sistema se usa por
ejemplo para las tablaturas de guitarra. Programar usando pattern matching la funcion:

cifradoAmericano :: NotaBasica -> Char

que relaciona las notas de Do, Re, Mi, Fa, Sol, La y Si con C, D, E, F, G, A, B respectivamente
-}

cifradoAmericano :: NotaBasica -> Char
cifradoAmericano Do  = 'C'
cifradoAmericano Re  = 'D'
cifradoAmericano Mi  = 'E'
cifradoAmericano Fa  = 'F'
cifradoAmericano Sol = 'G'
cifradoAmericano La  = 'A'
cifradoAmericano Si  = 'B'

{-
    EJEMPLOS DE EJECUCION
    ghci> cifradoAmericano La 
    'A'
    ghci> cifradoAmericano Mi
    'E'
    ghci> cifradoAmericano Sol
    'G'
-}



-- ######################################################################################
{-
2 . CLASES DE TIPOS
a) Completar la definicion del tipo NotaBasica para que las expresiones

    *Main> Do <= Re
    *Main> Fa `min` Sol

sean validas y no generen error. Ayuda: usar deriving con multiples clases.
-}

data NotaBasica = Do | Re | Mi | Fa | Sol | La | Si deriving (Show, Eq, Bounded, Ord )

{-
    EJEMPLOS DE EJECUCION
    ghci> Do <= Re
    True
    ghci> Fa `min` Sol
    Fa
-}



-- ######################################################################################

{-
3 . POLIMORFISMOS AD HOC
a) Definir usando polimorfismo ad hoc la funcion minimoElemento que calcula (de manera
recursiva) cual es el menor valor de una lista de tipo [a]. Asegurarse que solo este
definida para listas no vacias.
-}

minimoElemento :: Ord a => [a] -> a
minimoElemento [] = error "La lista està vacia, no se pueden comparar elementos"
minimoElemento [x] = x
minimoElemento (x:xs) = min x (minimoElemento xs)

{-
    EJEMPLOS DE EJECUCION
    ghci> minimoElemento [2,3,4,5]
    2
    ghci> minimoElemento [7,2,3,4,5]
    2
    ghci> minimoElemento [-7,-2,-8,-4,-5]
    -8
-}


-- //////////////////////////////////////////////////////////////////////////////////////
{-
3 . POLIMORFISMOS AD HOC
b) Definir la funcion minimoElemento’ de manera tal que el caso base de la recursion
sea el de la lista vacia. Para ello revisar la clase Bounded.
-}

minimoElemento' :: (Bounded a, Ord a) => [a] -> a
minimoElemento' [] = error "La lista esta vacia"
minimoElemento' (x:xs) = min x (minimoElemento xs)

{-
    EJEMPLOS DE EJECUCION
    ghci> let listaNotas = [Re, Sol, Fa, Do, Si, La, Mi]
    ghci> minimoElemento' listaNotas
    Do
-}


-- //////////////////////////////////////////////////////////////////////////////////////
{-
3 . POLIMORFISMOS AD HOC
c) Usar la funcion minimoElemento para determinar la nota mas grave de la melodia:
[Fa, La, Sol, Re, Fa]
-}

{-
    EJEMPLOS DE EJECUCION
    ghci> melodia = [Fa, La, Sol, Re, Fa]
    ghci> minimoElemento melodia
    Re
-}


-- ######################################################################################
{-
4 . SINONIMO DE TIPOS; CONSTRUCTORES CON PARAMETROS
a) Implementar el tipo Deportista y todos sus tipos accesorios 
(NumCamiseta, Altura, Zona, etc) tal como estan definidos arriba
-}

type Altura = Int
type NumCamiseta = Int

data Zona        = Arco      | Defensa | Mediocampo | Delantera  deriving (Show, Eq)
data TipoReves   = DosManos  | UnaMano                           deriving (Show, Eq)
data Modalidad   = Carretera | Pista   | Monte      | BMX        deriving (Show, Eq)
data PiernaHabil = Izquierda | Derecha                           deriving (Show, Eq)

type ManoHabil = PiernaHabil

data Deportista = Ajedrecista
                | Ciclista    Modalidad
                | Velocista   Altura
                | Tenista     TipoReves  ManoHabil    Altura
                | Futbolista  Zona       NumCamiseta  PiernaHabil  Altura

{-
    EJEMPLOS DE EJECUCION
    ghci> let ajedrecista = Ajedrecista
    ghci> let ciclista = Ciclista Carretera
    ghci> let velocista = Velocista 180
    ghci> let tenista = Tenista DosManos Izquierda 190
    ghci> let futbolista = Futbolista Defensa 10 Derecha 175
    ghci> ajedrecista
    Ajedrecista
    ghci> ciclista
    Ciclista Carretera
    ghci> velocista
    Velocista 180
    ghci> tenista
    Tenista DosManos Izquierda 190
    ghci> futbolista
    Futbolista Defensa 10 Derecha 175
    ghci> ajedrecista == ciclista
    False
    ghci> velocista == velocista
    True
    ghci> tenista == futbolista
    False
-}


-- //////////////////////////////////////////////////////////////////////////////////////
{-
4 . SINONIMO DE TIPOS; CONSTRUCTORES CON PARAMETROS
b) cual es el tipo del constructor de Ciclista?
-}

{-
    ghci> :t Ciclista
    Ciclista :: Modalidad -> Deportista
-}


-- //////////////////////////////////////////////////////////////////////////////////////
{-
4 . SINONIMO DE TIPOS; CONSTRUCTORES CON PARAMETROS
c) Programar la funcion contar_velocistas :: [Deportista] -> Int que dada una
lista de deportistas xs, devuelve la cantidad de velocistas que hay dentro de xs.
Programar contar_velocistas sin usar igualdad, utilizando pattern matching.
-}

contar_velocistas :: [Deportista] -> Int
contar_velocistas [] = 0
contar_velocistas (x:xs) = case x of
  Velocista _ -> 1 + contar_velocistas xs
  _ -> contar_velocistas xs

{-
    EJEMPLOS DE EJECUCION
    ghci> let deportistas = [Velocista 170, Futbolista Defensa 10 Derecha 180, Velocista 185, Ciclista Pista]
    ghci> contar_velocistas deportistas
    2
    ghci> let deportistas = [Velocista 170, Futbolista Defensa 10 Derecha 180, Velocista 185, Ciclista Pista, Velocista 190, Velocista 195]
    ghci> contar_velocistas deportistas
    4
    ghci> let deportistas = [Velocista 170, Futbolista Defensa 10 Derecha 180, Ciclista Pista]                                             
    ghci> contar_velocistas deportistas
    1
-}


-- //////////////////////////////////////////////////////////////////////////////////////
{-
4 . SINONIMO DE TIPOS; CONSTRUCTORES CON PARAMETROS
d) Programar la funcion contar_futbolistas :: [Deportista] -> Zona -> Int que
dada una lista de deportistas xs, y una zona z, devuelve la cantidad de futbolistas
incluidos en xs que juegan en la zona z. No usar igualdad, solo pattern matching.
-}

contar_futbolistas :: [Deportista] -> Zona -> Int
contar_futbolistas [] _ = 0
contar_futbolistas (x:xs) z = case x of
  Futbolista zonaDeportiva _ _ _ | zonaDeportiva == z -> 1 + contar_futbolistas xs z
  _ -> contar_futbolistas xs z

{-
    EJEMPLOS DE EJECUCION
    ghci> let deportistas = [Futbolista Defensa 10 Derecha 180, Futbolista Mediocampo 7 Izquierda 175, Ciclista Pista, Futbolista Delantera 9 Izquierda 182]
    ghci> contar_futbolistas deportistas Defensa
    1
    ghci> contar_futbolistas deportistas Delantera
    1
    ghci> contar_futbolistas deportistas Arco
    0
-}



-- ######################################################################################
{-
5 . DEFINICIÓN DE CLASES
a) implementar la funcion sonidoNatural como está definida arriba
-}

-- data NotaBasica = Do | Re | Mi | Fa | Sol | La | Si deriving (Show, Eq, Bounded, Ord)

sonidoNatural :: NotaBasica -> Int
sonidoNatural Do  = 0
sonidoNatural Re  = 2
sonidoNatural Mi  = 4
sonidoNatural Fa  = 5
sonidoNatural Sol = 7
sonidoNatural La  = 9
sonidoNatural Si  = 11

{-
    EJEMPLOS DE EJECUCION
    ghci> sonidoNatural Do
    0
    ghci> sonidoNatural La
    9
    ghci> sonidoNatural Si
    11
-}


-- //////////////////////////////////////////////////////////////////////////////////////
{-
5 . DEFINICIÓN DE CLASES
b) Definir el tipo enumerado Alteracion que consta de los constructores
Bemol, Natural y Sostenido
-}

data Alteracion = Bemol | Natural | Sostenido deriving (Show)

{-
    EJEMPLOS DE EJECUCION
    ghci> let bemol = Bemol 
    ghci> let natural = Natural
    ghci> let sostenido = Sostenido
    ghci> bemol
    Bemol
    ghci> natural
    Natural
    ghci> sostenido
    Sostenido
    ghci> natural == sostenido
    False
    ghci> bemol == Bemol
    True
-}


-- //////////////////////////////////////////////////////////////////////////////////////
{-
5 . DEFINICIÓN DE CLASES
c) Definir el tipo algebraico NotaMusical que debe tener un solo un constructor
que llamaremos Nota el cual toma dos parametros. El primer parametro es de tipo 
NotaBasica y el segundo de tipo Alteracion. De esta manera cuando se quiera 
representar una nota alterada se puede usar como segundo parametro del constructor 
un Bemol o Sostenido y si se quiere representar una nota sin alteraciones se usa 
Natural como segundo parametro.
-}

-- data NotaMusical = Nota NotaBasica Alteracion deriving (Show, Eq)

{-
    EJEMPLO DE EJECUCION
    ghci> let nota1 = Nota Do Natural  
    ghci> let nota2 = Nota Mi Sostenido
    ghci> let nota3 = Nota Fa Bemol    
    ghci> nota1                        
    Nota Do Natural
    ghci> nota2
    Nota Mi Sostenido
    ghci> nota3
    Nota Fa Bemol
    ghci> nota1 == nota2
    False
    ghci> nota1 == nota3
    False
-}


-- //////////////////////////////////////////////////////////////////////////////////////
{-
5 . DEFINICIÓN DE CLASES

d) definir la funcion sonidoCromatico :: NotaMusical -> Int
que devuelve el sonido de una nota, incrementando en uno su valor si tiene
la alteracion Sostenido, decrementando en uno si tiene la alteracion Bemol 
y dejando su valor intacto si la alteracion es Natural
-}

sonidoCromatico :: NotaMusical -> Int
sonidoCromatico (Nota nota alteracion) = case alteracion of
  Bemol -> sonidoNatural nota - 1
  Natural -> sonidoNatural nota
  Sostenido -> sonidoNatural nota + 1


{-
    EJEMPLOS DE EJECUCION

    ghci> let nota1 = Nota Do Natural
    ghci> let nota2 = Nota Re Bemol
    ghci> let nota3 = Nota Mi Sostenido
    ghci> sonidoCromatico nota1
    0
    ghci> sonidoCromatico nota2 
    1
    ghci> sonidoCromatico nota3
    5
-}


-- //////////////////////////////////////////////////////////////////////////////////////
{-
5 . DEFINICIÓN DE CLASES

e) Incluir el tipo NotaMusical a la clase Eq de manera tal que dos notas que tengan el
mismo valor de sonidoCromatico se consideren iguales.
-}

--data NotaMusical = Nota NotaBasica Alteracion deriving (Show)
--instance Eq NotaMusical where
--    nota1 == nota2 = sonidoCromatico nota1 == sonidoCromatico nota2

{-
    EJEMPLO DE EJECUCION
    ghci> let nota1 = Nota Mi Sostenido 
    ghci> let nota2 = Nota Fa Natural
    ghci> nota1 == nota2
    True
    ghci> let nota3 = Nota Do Bemol
    ghci> nota2 == nota3
    False
-}


-- //////////////////////////////////////////////////////////////////////////////////////
{-
5 . DEFINICIÓN DE CLASES

f)  Incluir el tipo NotaMusical a la clase Ord definiendo el operador <=. Se debe definir
que una nota es menor o igual a otra si y solo si el valor de sonidoCromatico para la
primera es menor o igual al valor de sonidoCromatico para la segunda.
-}

data NotaMusical = Nota NotaBasica Alteracion deriving (Show)
instance Eq NotaMusical where
    nota1 == nota2 = sonidoCromatico nota1 == sonidoCromatico nota2
instance Ord NotaMusical where
    nota1 <= nota2 = sonidoCromatico nota1 <= sonidoCromatico nota2


{-
    EJEMPLOS DE EJECUCION
    ghci> let nota1 = Nota Mi Sostenido 
    ghci> let nota2 = Nota Fa Natural
    ghci> nota1 == nota2
    True
    ghci> nota1 <= nota2
    True
    ghci> nota1 < nota2
    False
    ghci> let nota3 = Nota Do Bemol
    ghci> nota2 == nota3
    False
    ghci> nota3 <= nota2
    True
    ghci> nota2 >= nota3
    True
    ghci> nota2 <= nota3
    False
-}



-- ######################################################################################
{-
6 . TIPOS ENUMERADOS CON POLIMORFISMO 
a) Definir la funcion primerElemento que devuelve el primer elemento de una lista no
vacia, o Nothing si la lista es vacia.
-}

primerElemento :: [a] -> Maybe a 
primerElemento [] = Nothing
primerElemento (x:_) = Just x 

{-
    EJEMPLOS DE EJECUCION
    ghci> primerElemento [1, 2, 3]
    Just 1
    ghci> primerElemento []
    Nothing
-}



-- ######################################################################################
{-
7 . TIPOS RECURSIVOS
a) Programa las siguientes funciones:
-}

{-
a.1) atender :: Cola -> Maybe Cola, que elimina de la cola a la persona que est´a
en la primer posici´on de una cola, por haber sido atendida. Si la cola est´a vac´ıa,
devuelve Nothing.
-}

data Cola = VaciaC | Encolada Deportista Cola 

instance Show Cola where
  show VaciaC = "VaciaC"
  show (Encolada deportista resto) = "Encolada " ++ show deportista ++ " " ++ show resto

instance Show Deportista where
  show (Futbolista zona numCamiseta piernaHabil altura) =
    "Futbolista " ++ show zona ++ " " ++ show numCamiseta ++ " " ++ show piernaHabil ++ " " ++ show altura
  show (Ciclista modalidad) = "Ciclista " ++ show modalidad

atender :: Cola -> Maybe Cola
atender VaciaC = Nothing
atender (Encolada _ resto) = Just resto 

{-
    EJEMPLOS DE EJECUCION
    ghci> let colaDeportistas = Encolada (Futbolista Defensa 5 Izquierda 175) (Encolada (Ciclista Pista) VaciaC)
    ghci> let nuevaCola = atender colaDeportistas
    ghci> nuevaCola      
    Just Encolada Ciclista Pista VaciaC
-}


-- //////////////////////////////////////////////////////////////////////////////////////
{-
a.2) encolar :: Deportista -> Cola -> Cola, que agrega una persona a una cola
de deportistas, en la ultima posicion.
-}

encolar :: Deportista -> Cola -> Cola
encolar deportista VaciaC = Encolada deportista VaciaC
encolar deportista (Encolada d resto) = Encolada d (encolar deportista resto)

{-
    EJEMPLOS DE EJECUCION
    ghci> let colaInicial = Encolada (Futbolista Defensa 5 Izquierda 175) VaciaC
    ghci> let nuevaCola = encolar (Ciclista Pista) colaInicial
    ghci> nuevaCola
    Encolada Futbolista Defensa 5 Izquierda 175 Encolada Ciclista Pista VaciaC
-}


-- //////////////////////////////////////////////////////////////////////////////////////
{-
a.3) busca :: Cola -> Zona -> Maybe Deportista, que devuelve el/la primera
futbolista dentro de la cola que juega en la zona que se corresponde con el segundo
parametro. Si no hay futbolistas jugando en esa zona devuelve Nothing.
-}

busca :: Cola -> Zona -> Maybe Deportista
busca VaciaC _ = Nothing
busca (Encolada deportista resto) zona =
  case deportista of
    Futbolista zonaDeportiva _ _ _ | zonaDeportiva == zona -> Just deportista
    _ -> busca resto zona

{-
    EJEMPLOS DE EJECUCION
    ghci> let colaDeportistas = Encolada (Futbolista Defensa 5 Izquierda 175) (Encolada (Futbolista Delantera 10 Derecha 180) VaciaC)
    ghci> let futbolistaEnZona = busca colaDeportistas Delantera
    ghci> futbolistaEnZona
    Just Futbolista Delantera 10 Derecha 180
-}


-- //////////////////////////////////////////////////////////////////////////////////////
{-
7 . TIPOS RECURSIVOS
b) ¿A que otro tipo se parece Cola?

con el constructor Encolada recibiendo como parametros a Deportista y recursivamente Cola,
se parece al tipo Deportista, ambos tipos algebraicos con constructores parametricos
-}


-- ######################################################################################
{-
8 . TIPOS RECURSIVOS Y POLIMORFICOS
a) ¿Como se debe instanciar el tipo ListaAsoc para representar la informacion almacenada
en una guia telefonica?
-}

--type guiaTelefonica = ListaAsoc String String 

--donde el primer String representa el nombre de la persona y el segundo String su numero de telefono


-- //////////////////////////////////////////////////////////////////////////////////////
{-
8 . TIPOS RECURSIVOS Y POLIMORFICOS
b) Programar las siguientes funciones
-}

{-
8 . TIPOS RECURSIVOS Y POLIMORFICOS
b.1) la_long :: ListaAsoc a b -> Int que devuelve la cantidad de datos en una
lista
-}

data ListaAsoc a b = Vacia | Nodo a b (ListaAsoc a b) deriving (Show)

la_long :: ListaAsoc a b -> Int
la_long Vacia = 0 
la_long (Nodo _ _ resto) = 1 + la_long resto 

{-
    EJEMPLOS DE EJECUCION
    ghci> let listaEjemplo = Nodo "Dato1" 1 (Nodo "Dato2" 2 (Nodo "Dato3" 3 Vacia))
    ghci> let longitud = la_long listaEjemplo
    ghci> longitud
    3    
-}


-- //////////////////////////////////////////////////////////////////////////////////////
{-
8 . TIPOS RECURSIVOS Y POLIMORFICOS
b.2) la_concat :: ListaAsoc a b -> ListaAsoc a b -> ListaAsoc a b, que devuelve la concatenacion de dos listas de asociaciones
-}

la_concat :: ListaAsoc a b -> ListaAsoc a b -> ListaAsoc a b
la_concat Vacia lista2 = lista2 -- Si la primera lista está vacía, devolvemos la segunda
la_concat (Nodo a b resto1) lista2 = Nodo a b (la_concat resto1 lista2)

{-
    EJEMPLOS DE EJECUCION
    ghci> let lista1 = Nodo "Dato1" 1 (Nodo "Dato2" 2 Vacia)
    ghci> let lista2 = Nodo "Dato3" 3 (Nodo "Dato4" 4 Vacia)
    ghci> let resultado = la_concat lista1 lista2
    ghci> resultado
    Nodo "Dato1" 1 (Nodo "Dato2" 2 (Nodo "Dato3" 3 (Nodo "Dato4" 4 Vacia)))
-}


-- //////////////////////////////////////////////////////////////////////////////////////
{-
8 . TIPOS RECURSIVOS Y POLIMORFICOS
b.3) la_agregar :: Eq a => ListaAsoc a b -> a -> b -> ListaAsoc a b, que
agrega un nodo a la lista de asociaciones si la clave no esta en la lista, o actualiza
el valor si la clave ya se encontraba.
-}

la_agregar :: Eq a => ListaAsoc a b -> a -> b -> ListaAsoc a b
la_agregar Vacia nuevaClave nuevoValor = Nodo nuevaClave nuevoValor Vacia
la_agregar (Nodo clave valor resto) nuevaClave nuevoValor
  | clave == nuevaClave = Nodo clave nuevoValor resto 
  | otherwise = Nodo clave valor (la_agregar resto nuevaClave nuevoValor) 

{-
    EJEMPLOS DE EJECUCION
    ghci> let listaInicial = Nodo "clave1" "valor1" (Nodo "clave2" "valor2" Vacia)
    ghci> let listaActualizada = la_agregar listaInicial "clave3" "valor3"
    ghci> listaActualizada
    Nodo "clave1" "valor1" (Nodo "clave2" "valor2" (Nodo "clave3" "valor3" Vacia))
-}


-- //////////////////////////////////////////////////////////////////////////////////////
{-
8 . TIPOS RECURSIVOS Y POLIMORFICOS
b.4) la_pares :: ListaAsoc a b -> [(a, b)] que transforma una lista de asociaciones en una lista de pares clave-dato
-}

la_pares :: ListaAsoc a b -> [(a, b)]
la_pares Vacia = [] 
la_pares (Nodo clave valor resto) = (clave, valor) : la_pares resto

{-
    EJEMPLOS DE EJECUCION
    ghci> let listaAsociativa = Nodo "clave1" "valor1" (Nodo "clave2" "valor2" Vacia)
    ghci> let listaDePares = la_pares listaAsociativa
    ghci> listaDePares
    [("clave1","valor1"),("clave2","valor2")]
-}


-- //////////////////////////////////////////////////////////////////////////////////////
{-
8 . TIPOS RECURSIVOS Y POLIMORFICOS
b.5) la_busca :: Eq a => ListaAsoc a b -> a -> Maybe b que dada una lista
y una clave devuelve el dato asociado, si es que existe. En caso contrario devuelve
Nothing
-}

la_busca :: Eq a => ListaAsoc a b -> a -> Maybe b
la_busca Vacia _ = Nothing 
la_busca (Nodo clave valor resto) claveBuscada
  | clave == claveBuscada = Just valor 
  | otherwise = la_busca resto claveBuscada 

{-
    EJEMPLOS DE EJECUCION
    ghci> let listaAsociativa = Nodo "clave1" "valor1" (Nodo "clave2" "valor2" Vacia)
    ghci> let resultado1 = la_busca listaAsociativa "clave1" -- Debería devolver Just "valor1"
    ghci> let resultado2 = la_busca listaAsociativa "clave3" -- Debería devolver Nothing
    ghci> resultado1
    Just "valor1"
    ghci> resultado2
    Nothing
-}

-- //////////////////////////////////////////////////////////////////////////////////////
{-
8 . TIPOS RECURSIVOS Y POLIMORFICOS
b.6) la_borrar :: Eq a => a -> ListaAsoc a b -> ListaAsoc a b que dada
una clave a elimina la entrada en la lista.
-}

la_borrar :: Eq a => a -> ListaAsoc a b -> ListaAsoc a b
la_borrar _ Vacia = Vacia 
la_borrar clave (Nodo claveActual _ resto)
  | clave == claveActual = resto 
  | otherwise = Nodo claveActual valorActual (la_borrar clave resto)
  where
    (claveActual, valorActual) = buscarNodo clave resto

-- Función auxiliar para buscar un nodo en la lista y obtener su valor
buscarNodo :: Eq a => a -> ListaAsoc a b -> (a, b)
buscarNodo clave (Nodo claveActual valorActual resto)
  | clave == claveActual = (claveActual, valorActual)
  | otherwise = buscarNodo clave resto


-- ######################################################################################


