module Maze where
{-
Version 1.0 
Modulo para la resolucion de caminos de laberintos
-}

--Localiza el inicio del laberinto marcado con una X y llama a la ruta para generar el conjunto de coordenadas a recorrer
inicio::[[Char]]->Int->Int->[[Int]]
inicio arr i j = if((arr!!j!!i) == 'X') 
then concat [[[i,j]],[(ruta arr i j)]]
else if i<((length (arr!!j))-1) then inicio arr (i+1) j
else inicio arr 0 (j+1)

--Ruta que llama a las funciones mirar Arriba,Abajo, Izquierda y Derecha para determinar donde continua el camino,
--termina cuando encuentra la posicion que contiene el final marcado con F
--Presenta Error: parse error (possibly incorrect identation or mismatched brackets)
ruta::[[Char]]->Int->Int->[[Int]] ruta arr i j = if (arr!!j!!i)=='F' then [] else 
if (lookUp arr i j) then concat [[[i,j-1]],[(ruta arr i (j-1))]] 
else if (lookDown arr i j) then concat [[[i,j+1]],[(ruta arr i (j+1))]]
else if (lookLeft arr i j) then concat [[[i-1,j]],[(ruta arr (i-1) j)]]
else if (lookRight arr i j) then concat [[[i+1,j]],[(ruta arr (i+1) j)]]
else [[]]

--Funciones para ver los valores adyacentes a una posicion especifica.
lookUp::[[Char]]->Int->Int->Bool
lookUp arr i j = if ((arr!!(j-1)!!i)=='0' || (arr!!(j-1)!!i)=='F') then True else False

lookDown::[[Char]]->Int->Int->Bool
lookDown arr i j = if ((arr!!(j+1)!!i)=='0' || (arr!!(j+1)!!i)=='F') then True else False

lookLeft::[[Char]]->Int->Int->Bool
lookLeft arr i j = if ((arr!!j!!(i-1))=='0' || (arr!!j!!(i-1))=='F') then True else False

lookRight::[[Char]]->Int->Int->Bool
lookRight arr i j = if ((arr!!j!!(i+1))=='0' || (arr!!j!!(i+1))=='F') then True else False
