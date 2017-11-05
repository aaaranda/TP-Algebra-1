module Aritmetica where
import Catedra
import Data.Tuple
import Data.Bits


--(1)
mcdExt :: Integer -> Integer -> (Integer, (Integer, Integer))
mcdExt _ _ = (0, (0, 0))

--(2)
criba :: Integer -> Set Integer
criba x    | x == 1 = []
           | esPrimo x = reverse (x : criba (x-1))
           | otherwise = reverse(criba (x-1))

--(3)
coprimoCon:: Integer -> Integer
coprimoCon _ = 0


--(4)
inversoMultiplicativo:: Integer -> Integer -> Integer
inversoMultiplicativo _ _ = 0



-- Función de regalo para exponenciar "rápido"
modExp :: Integer -> Integer -> Integer -> Integer
modExp b 0 m = 1
modExp b e m = t * modExp ((b * b) `mod` m) (shiftR e 1) m `mod` m
  where t = if testBit e 0 then b `mod` m else 1

-- Funciones auxiliares para criba  

esPrimo :: Integer -> Bool
esPrimo x    	| (sumaDivisores x) == (x+1) = True
                | otherwise = False
 
sumaDivisoresHasta :: Integer -> Integer -> Integer
sumaDivisoresHasta x y 		| y == 1 = 1
                            | x `mod` y == 0 = y + sumaDivisoresHasta x (y-1)
                            | otherwise = sumaDivisoresHasta x (y-1)
 
sumaDivisores x = sumaDivisoresHasta x x

-- Fin funciones auxiliares criba
