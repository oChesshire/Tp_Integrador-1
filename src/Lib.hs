module Lib where
import Text.Show.Functions

laVerdad = True

type Desgaste = Float
type Patente = String
type Fecha = (Int, Int, Int)
 
-- Definiciones base
anio :: Fecha -> Int
anio (_, _, year) = year
 
data Auto = Auto {
 patente :: Patente,
 desgasteLlantas :: [Desgaste],
 rpm :: Int,
 temperaturaAgua :: Int,
 ultimoArreglo :: Fecha
} deriving Show

-- PUNTO 1 --
{- Saber el costo de reparación de un auto:
· si la patente tiene 7 dígitos, es $ 12.500
· si no, si la patente está entre las letras "DJ" y "NB", se aplica el calculoPatental que es
 $ 3.000 * la longitud para las patentes que terminen en 4 o $ 20.000 para el resto de las patentes
· de lo contrario, se le cobra $ 15000 -}

primerasDos :: Patente -> String
primerasDos patente = take 2 (patente) 

estaEntre :: Patente -> Bool
estaEntre patente = primerasDos patente > "DJ" && primerasDos patente < "NB"

terminaEnCuatro :: Patente -> Bool
terminaEnCuatro = (=='4').last

calculoPatental :: Patente -> Int
calculoPatental patente 
 | terminaEnCuatro patente =  length patente * 3000
 | otherwise  = 20000



costoAuto :: Auto -> Int 
costoAuto auto
 | (length.patente $ auto) == 7 = 12500
 | estaEntre.patente $ auto = calculoPatental.patente $ auto
 | otherwise = 15000




auto1 :: Auto
auto1 = Auto "AT001LN" [0.5,0.3,0.8,0] 0 0 (0,0,0)
auto2 :: Auto
auto2 = Auto "DJV214" [] 0 0 (0,0,0)
auto3 :: Auto
auto3 = Auto "DJV215" [] 0 0 (0,0,0)
auto4 :: Auto
auto4 = Auto "DFH029" [] 0 0 (0,0,0)



--Punto 2--

-- Parte 1 -- Auto peligroso 
-- Dado un auto, saber si es peligroso. Esta condición se cumple cuando el desgaste de 
-- la primera llanta es mayor a 0.5


primeraLlanta :: Auto -> Float
primeraLlanta = head.desgasteLlantas

autoPeligroso :: Auto -> Bool
autoPeligroso = (>0.5).primeraLlanta

-- Parte 2 ) Necesita revisión (integrante b)
-- Dado un auto, saber si necesita revisión. Esta condición se cumple cuando el último 
-- arreglo fue realizado en el año 2015 ó antes.

necesitaRevision :: Auto -> Bool
necesitaRevision = (<=2015).anio.ultimoArreglo

{-
Punto 3: Personal técnico encargado de las reparaciones
 Parte 1 Integrante a
Necesitamos definir a las siguientes personas que realizan actividades en el taller mecánico:
Alfa: hace que el auto regule a 2.000 vueltas, salvo que esté a menos de 2.000 vueltas
en cuyo caso lo deja como está
Bravo: cambia todas las cubiertas, dejándolas sin desgaste
Charly:  realiza las mismas actividades que Alfa y Bravo
-}


type Tecnico = Auto -> Auto

tecnicoAlfa :: Tecnico
tecnicoAlfa auto
 | (>2000).rpm $ auto = auto {rpm = 2000}
 | otherwise = auto

tecnicoBravo :: Tecnico
tecnicoBravo auto = auto {desgasteLlantas = [0,0,0,0]}

tecnicoCharly :: Tecnico
tecnicoCharly = tecnicoBravo.tecnicoBravo

{-
Parte 2) Integrante b
Necesitamos definir a las siguientes personas que realizan actividades en el taller mecánico
Tango: le gusta decir que hizo muchas cosas pero en realidad no hace ningún arreglo
Zulu: revisa la temperatura del agua, la deja a 90 y hace lo mismo que Lima (ver a continuación)
Lima:  cambia las cubiertas delanteras (las dos primeras), dejándolas sin desgaste. Las posteriores quedan igual
-}

tecnicoTango :: Tecnico
tecnicoTango = id

cambiarPrimerasDos :: [Desgaste] -> [Desgaste]
cambiarPrimerasDos (llantaA : llantaB : resto) = (0 : 0 : resto)

tecnicoLima :: Tecnico
tecnicoLima auto = auto { desgasteLlantas = cambiarPrimerasDos (desgasteLlantas auto)}

tecnicoZulu :: Tecnico
tecnicoZulu auto = tecnicoLima (auto {temperaturaAgua = 90})



{-
Punto 4: Ordenamiento TOC de autos

(Común para ambos integrantes) 
Dada una serie de autos, saber si están ordenados en base al siguiente criterio:
los autos ubicados en la posición impar de la lista deben tener una cantidad de desgaste impar
los autos ubicados en la posición par deben tener una cantidad de desgaste par
asumimos que el primer elemento está en la posición 1, el segundo elemento en la posición 2, etc.

La cantidad de desgaste es la sumatoria de desgastes de las cubiertas de los autos multiplicada por 10.
Ejemplo: 0.2 + 0.5 + 0.6 + 0.1 = 1.4 * 10 = 14. Para determinar si es par o no (y evitar errores de redondeo) 
 es conveniente utilizar la función round.

 
Solamente se puede utilizar recursividad en este punto.

BONUS: Evitar repetición de código.

-}

desgasteTotal :: Auto -> Int
desgasteTotal  = round.(*10).(sum.desgasteLlantas)

posImpar :: Auto -> Bool
posImpar = odd.desgasteTotal

posPar :: Auto -> Bool
posPar = even.desgasteTotal

ordenarTOC :: [Auto] -> Bool
ordenarTOC [] = True
ordenarTOC [autoA]= posImpar autoA 
ordenarTOC [autoA, autoB] = posImpar autoA && posPar autoB
ordenarTOC (autoA : autoB : restoAutos) = posImpar autoA && posPar autoB && ordenarTOC restoAutos

autoa :: Auto
autoa = Auto "AT001LN"  [0.1, 0.4, 0.2, 0] 0 0 (0,0,0)
autob :: Auto
autob = Auto "AT001LN"  [0.2, 0.5, 0.6, 0.1] 0 0 (0,0,0) 
autoc :: Auto
autoc = Auto "AT001LN"  [0.1, 0.1, 0.1, 0] 0 0 (0,0,0)
autod :: Auto
autod = Auto "AT001LN"  [0.1, 0.4, 0.2, 0.1] 0 0 (0,0,0)
autof :: Auto
autof = Auto "AT001LN"  [0.1, 0.4, 0.2, 0] 0 0 (0,0,0)
autog :: Auto
autog = Auto "AT001LN"  [0.3, 0.5, 0.6, 0] 0 0 (0,0,0)
autoh :: Auto
autoh = Auto "AT001LN"  [0.1, 0.1, 0.1, 0.1] 0 0 (0,0,0)
autoi :: Auto
autoi = Auto "AT001LN"  [0.2, 0.4, 0.2, 0] 0 0 (0,0,0)
autoj :: Auto
autoj = Auto "AT001LN"  [0.1, 0.4, 0.2, 0.1] 0 0 (0,0,0)
lista1 :: [Auto]
lista1 = [autoa,autob,autoc,autod]
lista2 :: [Auto]
lista2 = [autof,autog,autoh]

{-
Punto 5: Orden de reparación
(Común para ambos integrantes) 
Aplicar una orden de reparación, que tiene
una fecha
una lista de técnicos
y consiste en que cada uno de los técnicos realice las reparaciones que sabe sobre el auto,
al que además hay que actualizarle la última fecha de reparación.

-}

ordenReparacion :: Auto -> Fecha -> [Tecnico] -> Auto
ordenReparacion unAuto  unaFecha listaTecnicos = (foldr1 (.) listaTecnicos unAuto) {ultimoArreglo = unaFecha} 



{-
Punto 6

Solamente se puede utilizar funciones de orden superior en este punto.

Parte 1) Integrante a: Técnicos que dejan el auto en condiciones
Dada una lista de técnicos determinar aquellos técnicos que dejarían el auto en condiciones 
    (que no sea peligroso andar, recordar el punto 2.1 del integrante a).

-}

dejaEnCondiciones :: Auto -> [Tecnico] -> [Tecnico]
dejaEnCondiciones unAuto tecnicos = filter (\tecnicos -> (<0.5).primeraLlanta.tecnicos $ unAuto ) tecnicos 


{-
Parte 2) Integrante b: Costo de reparación de autos que necesitan revisión
Dada una lista de autos, saber cuál es el costo de reparación de los autos que necesitan revisión.
-}

costoReparacion :: [Auto] -> [Int]
costoReparacion listaAutos = map (costoAuto) (filter (necesitaRevision) listaAutos)
