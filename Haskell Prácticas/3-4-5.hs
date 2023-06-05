doubleMe :: Int -> Int
doubleMe x = x + x

--------------------------  PRACTICA 3 --------------------------------------------------------

--EJERCICIO 1

fparcial ::Integer -> Integer
fparcial n | n == 1 = 8
           | n == 4 = 131
           | n == 16 = 16

gparcial :: Integer -> Integer
gparcial n | n== 8 = 16
           | n== 16 = 4 
           | n== 131 = 1

hcompuesta:: Integer -> Integer
hcompuesta n = fparcial(gparcial n)

kcompuesta :: Integer -> Integer
kcompuesta n = gparcial(fparcial n)

--EJERCICIO 2

absoluto :: Integer -> Integer
absoluto n = if n >= 0 then n else -n 


maximoabsoluto  :: Integer -> Integer -> Integer
maximoabsoluto x y | x < 0 && y < 0 = if x > y then y else x
                   | x < 0 = if -x > y then x else y
                   | y < 0 = if x > -y then x else y
                   | otherwise = if x > y then  x else y

maximo3 :: Int -> Int -> Int -> Int
maximo3 a b c | a >= b && a >= c = a
              | b >= c          = b
              | otherwise       = c


algunoEs0      :: Float -> Float -> Bool
algunoEs0 x y  = x==0 || y==0 

algunoEs02 :: Float -> Float -> Bool
algunoEs02 0 _ = True
algunoEs02 _ 0 = True
algunoEs02 _ _ = False

ambosSon0 ::  Float -> Float -> Bool
ambosSon0 a b = a == 0 && b == 0

ambosSon01 :: Float -> Float -> Bool
ambosSon01 0 0 = True
ambosSon01 _ _ = False

mismoIntervalo :: Float -> Float -> Bool
mismoIntervalo x y | x <= 3 && y <= 3 = True
                   | x > 3 && x <= 7 && y > 3 && y <= 7 = True
                   | x > 7 && y > 7 = True
                   | otherwise = False

sumaDistintos :: Int -> Int -> Int -> Int    
sumaDistintos a b c | a == b  && b == c =a  
                    | a == b = c          
                    | b == c = a  
                    | a == c = b    
                    | otherwise = a + b + c      

esMultiploDe :: Int -> Int -> Bool
esMultiploDe x y | x >= y = True
                 | otherwise = False

digitoUnidades :: Int -> Int   --último dígito
digitoUnidades x = x `mod` 10  --La función digitoUnidades toma un número natural x y aplica la operación módulo (mod) con el número 10 para obtener el dígito en la posición de las unidades

digitoDecenas :: Int -> Int  -- anteultimo dígito
digitoDecenas x = (x `div` 10) `mod` 10

-- EJERCICIO 3

estanRelacionados :: Integer -> Integer -> Bool
estanRelacionados a b = a == 0 && b == 0 || mod (a^2) (a*b) == 0

-- EJERCICIO 4

prodInt :: (Double, Double) -> (Double, Double) -> Double
prodInt (a, b) (c, d) = a*c + b*d

todoMenor :: (Float, Float) -> (Float, Float) -> Bool
todoMenor (a,b) (c,d) |a < c && b < d = True
                      |a >= c && b >= d = False


distanciaPuntos :: (Double, Double) -> (Double, Double) -> Double
distanciaPuntos (x1, y1) (x2, y2) = sqrt((x2 - x1)^2 + (y2 - y1)^2)  -- distancia entre dos puntos =√((x2-x1)²+(y2-y1)²)

-- 'sqrt' se utiliza para calcular la raíz cuadrada de un número
                      
sumaTerna :: (Int, Int, Int) -> Int   --Toma una terna de enteros y devuelve su suma.
sumaTerna (a,b,c) = a + b + c 

--sumarSoloMultiplos :: (Int,Int,Int) -> Int -> Int
--sumarSoloMultiplos (a,b,c) d | (preguntar!)

posPrimerpar :: (Int, Int, Int) -> Int
posPrimerpar (x, y, z) 
        | mod x 2 == 0 = 1
        | mod y 2 == 0 = 2
        | mod z 2 == 0 = 3
        | otherwise = 4
--Toma como posición del primer número al primer digito , es decir, 1-2-3 , no 0-1-2-3 << recordatorio!

crearPar :: a -> b -> (a, b)
crearPar x y = (x, y)

invertir :: (a, b)->(b, a)
invertir (a,b) = (b,a)


--EJERCICIO 5

todosMenores :: (Integer, Integer, Integer) -> Bool
todosMenores (n1, n2, n3) = f n1 > g n1 && f n2 > g n2 && f n3 > g n3
  where
    f n
      | n <= 7 = n ^ 2
      | otherwise = 2 * n - 1
    g n
      | esPar n = n `div` 2
      | otherwise = 3 * n + 1
    esPar n = n `mod` 2 == 0

--EJERCICIO 6
 
esMultiplo1 :: Integer -> Integer -> Bool
esMultiplo1 x y = x `mod` y == 0
 
bisiesto :: Integer -> Bool
bisiesto año 
    | not (esMultiplo1 año 4) = False
    | esMultiplo1 año 100 = esMultiplo1 año 400
    | otherwise = True

--EJERCICIO 7

distanciaManhattan :: (Float,Float,Float) -> (Float,Float,Float) -> Float
distanciaManhattan (x1, y1, z1) (x2, y2, z2) = abs(x1 - x2) + abs(y1 - y2) + abs(z1 - z2)

--EJERCICIO 8


sumaUltimosDosDigitos :: Int -> Int
sumaUltimosDosDigitos x = (x `mod`10) + ((x `div` 10) `mod` 10)

comparar :: Int -> Int -> Int
comparar a b 
          | (sumaUltimosDosDigitos a < sumaUltimosDosDigitos b) = 1
          | (sumaUltimosDosDigitos a > sumaUltimosDosDigitos b) = -1
          | otherwise= 0


--------------------- PRACTICA 4 --------------------------------------------------------------------------------------

--EJERCICIO 1

fibonacci :: Integer -> Integer
fibonacci n
        | n == 0 = 0
        | n == 1 = 1
        | otherwise = fibonacci (n-1) + fibonacci (n-2)

--EJERCICIO 2        

parteEntera :: Float -> Integer
parteEntera x = floor x --La función "floor" redondea hacia abajo un número flotante y devuelve su parte entera

--EJERCICIO 3                                                                                                             

esDivisible :: Integer -> Integer -> Bool
esDivisible x y | x<y =False --'si el divisor es menor que el dividendo'
                | x==y = True
                | x <=0 =False --'deben ser números naturales' - preguntar si dejarlo como false o como exception
                | otherwise = esDivisible (x - y) y
--La función tiene tres casos base y un caso recursivo

--EJERCICIO 4

sumaImpares :: Integer -> Integer
sumaImpares 0 = 0
sumaImpares n =(2*n-1) + sumaImpares (n-1) -- ' 1+3+...+ (2*n-1), lo sumamos con la suma de los primeros (n-1) números impares

--EJERCICIO 5

medioFact :: Integer -> Integer
medioFact 0 = 1
medioFact 1 = 1
medioFact n = n * medioFact (n-2)

---------------
--Otra opción--
---------------

medioFact2 :: Integer -> Integer
medioFact2 n
    | n <= 0 = 1
    | otherwise = product [n, n-2..1]

--EJERCICIO 6
 
sumaDigitos :: Integer -> Integer
sumaDigitos n
  | n < 10    = n  -- se debe a que un número menor que 10 solo tiene un dígito en su representación decimal, por lo que su suma de dígitos es igual
  | otherwise = (n `mod` 10) + sumaDigitos (n `div` 10)   -- se suman los dígitos del número utilizando el mód para obtener el último dígito 
                                                          -- y la div  para obtener el resto del número

--EJERCICIO 7

todosDigitosIguales :: Integer -> Bool
todosDigitosIguales n | n < 10 = True
                      | otherwise = (ultimoDigito n)  == (ultimoDigito (n `div` 10)) && todosDigitosIguales (n `div` 10)                                                     
 where ultimoDigito n = n `mod` 10

--EJERCICIO 8

cantDigitos :: Integer -> Integer
cantDigitos n
    | n < 10 = 1
    | otherwise = 1 + cantDigitos (n `div` 10)

iesimoDigito :: Integer -> Integer -> Integer
iesimoDigito n i = (n `div` (10^(cantDigitos n - i))) `mod` 10    

--EJERCICIO 10

f1::Integer -> Integer
f1 0=1
f1 n=2^n +f1(n-1)

f2 :: Integer -> Float -> Float 
f2 n q
    | n ==0=1
    |otherwise = (q^n) + f2(n-1) q

f3 :: Integer -> Float -> Float
f3 n q
    | n==0=1
    | otherwise= q^(2*n) +f3(n-1)q+ q^(2*n-1)

f4 :: Integer -> Float -> Float
f4 n q
    | n == 0 = q^n
    | otherwise= q^(2*n)+ f4(n-1)q 

-----------------------------------

-----------------------
--    PRACTICA 5     --
-----------------------

todosDistintos1 :: (Eq t) => [t] -> Bool 
todosDistintos1 []= True 
todosDistintos1 (x:xs)
    | pertenece x xs = False

-- EJERCICIO 1 --

--1.1
longitud :: [t] -> Integer
longitud [] = 0
longitud (x:xs)= 1 + longitud xs


--1.2
ultimo :: [t] -> t
ultimo []= error "no definido"
ultimo [x]= x
ultimo (_:xs)=ultimo xs

--1.3
principio :: [t] -> [t]
principio [] = error "La lista está vacía"
principio [x] = []
principio (x:xs) = x : principio xs

--1.4
reverso :: [t] -> [t]
reverso [] = []
reverso (x:xs)=reverso xs ++ [x]

--EJERCICIO 2

--2.1
pertenece :: Eq t => t -> [t] -> Bool
pertenece _ [] = False
pertenece e (x:xs)
  | e == x = True
  | otherwise = pertenece e xs

--pertenece1 _[]= False
--pertenece1 e(x:xs)= e == x|| (pertenece e xs)

--2.2
todosIguales :: (Eq t) => [t] -> Bool
todosIguales [] = True        -- caso base: una lista vacía tiene elementos iguales
todosIguales [_] = True       -- caso base: una lista con un solo elemento tiene elementos iguales
todosIguales (x:y:xs) = (x == y) && todosIguales (y:xs) -- recursión: comparamos el primer elemento con el segundo, y el segundo con el tercero, y así sucesivamente

--2.3
todosDistintos :: (Eq t) => [t] -> Bool
todosDistintos [] = True
todosDistintos (x:xs) = not (pertenece x xs) && todosDistintos xs

--2.4
hayRepetidos :: (Eq t) => [t] -> Bool
hayRepetidos [] = False
hayRepetidos (x:xs) = pertenece x xs || hayRepetidos xs

--2.5
quitar :: (Eq t) => t -> [t] -> [t]
quitar _ [] = []
quitar x (y:ys)
  | x == y    = ys
  | otherwise = y : quitar x ys

--2.6
quitarTodos :: (Eq t) => t -> [t] -> [t]
quitarTodos e [] = []
quitarTodos e (x:xs)
  | e == x = quitarTodos e xs
  | otherwise = x : quitarTodos e xs

--2.7
eliminarRepetidos :: Eq t => [t] -> [t]
eliminarRepetidos [] = []
eliminarRepetidos (x:xs) = x : eliminarRepetidos (quitar x xs)  

mismosElementos :: (Eq t) => [t] -> [t] -> Bool
mismosElementos xs ys = eliminarRepetidos xs == eliminarRepetidos ys

--2.9
capicua2 :: (Eq t) => [t] -> Bool
capicua2 xs = xs == reverso xs





--EJERCICIO 3

--maximo :: [Integer] -> Integer
--maximo (x:xs): 
