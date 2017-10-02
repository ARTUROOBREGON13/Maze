module Maze where
{-
Version 1.0 
Modulo para la resolucion de caminos de laberintos
-}

--Localiza el inicio del laberinto marcado con una X y llama a la ruta para generar el conjunto de coordenadas a recorrer
inicio::[[Char]]->Int->Int->[Char]
inicio arr i j = if((arr!!j!!i) == 'X') then (ruta3 arr i j [i,j])
                 else if i<((length (arr!!j))-1) then inicio arr (i+1) j
                      else inicio arr 0 (j+1)

--Ruta que llama a las funciones mirar Arriba,Abajo, Izquierda y Derecha para determinar donde continua el camino,
--termina cuando encuentra la posicion que contiene el final marcado con F
ruta3::[[Char]]->Int->Int->[Int]->[Char] 
ruta3 arr i j ant |(arr!!j!!i) == 'F' = "FIN" 
                 |lookUp arr i j ant = concat ["Arriba - ",(ruta3 arr i (j-1) [i,j])] 
                 |lookDown arr i j ant = concat ["Abajo - ",(ruta3 arr i (j+1) [i,j])]    
                 |lookLeft arr i j ant  = concat ["Izquierda - ",(ruta3 arr (i-1) j [i,j])]   
                 |lookRight arr i j ant = concat ["Derecha - ",(ruta3 arr (i+1) j [i,j])]                           

--Funciones para ver los valores adyacentes a una posicion especifica.
lookUp::[[Char]]->Int->Int->[Int]->Bool
lookUp arr i j ant = if ((arr!!(j-1)!!i)=='0' || (arr!!(j-1)!!i)=='F') && ([i,(j-1)] /= ant) then True else False

lookDown::[[Char]]->Int->Int->[Int]->Bool
lookDown arr i j ant = if ((arr!!(j+1)!!i)=='0' || (arr!!(j+1)!!i)=='F') && ([i,(j+1)] /= ant) then True else False

lookLeft::[[Char]]->Int->Int->[Int]->Bool
lookLeft arr i j ant = if ((arr!!j!!(i-1))=='0' || (arr!!j!!(i-1))=='F') && ([(i-1),j] /= ant) then True else False

lookRight::[[Char]]->Int->Int->[Int]->Bool
lookRight arr i j ant = if ((arr!!j!!(i+1))=='0' || (arr!!j!!(i+1))=='F') && ([(i+1),j] /= ant) then True else False

--Hallar Elemento F
fin::[[Char]]->Int->Int->[Int]
fin arr i j = if((arr!!j!!i) == 'F') then [i,j]
                 else if i<((length (arr!!j))-1) then fin arr (i+1) j
                      else fin arr 0 (j+1)
                      
hallarFin::[[Char]]->[Int]
hallarFin arr = fin arr 0 0

