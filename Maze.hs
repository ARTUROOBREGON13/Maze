module Maze where
{-
Version 1.0 
Modulo para la resolucion de caminos de laberintos
-}

--Localiza el inicio del laberinto marcado con una X y llama a la ruta para generar el conjunto de coordenadas a recorrer
inicio::[[Char]]->Int->Int->[[Int]]
inicio arr i j = if((arr!!j!!i) == 'X') then concat [[[i,j]], (ruta arr i j)]
                 else if i<((length (arr!!j))-1) then inicio arr (i+1) j
                      else inicio arr 0 (j+1)

--Ruta que llama a las funciones mirar Arriba,Abajo, Izquierda y Derecha para determinar donde continua el camino,
--termina cuando encuentra la posicion que contiene el final marcado con F
ruta::[[Char]]->Int->Int->[[Int]] 
ruta arr i j  |lookUp arr i j ([i,j]) = concat [[[i,j-1]],(ruta2 arr i (j-1) [i,j])] 
              |lookDown arr i j [i,j] = concat [[[i,j+1]],(ruta2 arr i (j+1) [i,j])]    
              |lookLeft arr i j [i,j] = concat [[[i-1,j]],(ruta2 arr (i-1) j [i,j])]   
              |lookRight arr i j [i,j] = concat [[[i+1,j]],(ruta2 arr (i+1) j [i,j])]

ruta2::[[Char]]->Int->Int->[Int]->[[Int]] 
ruta2 arr i j ant |(arr!!j!!i) == 'F' = [[]] 
                 |lookUp arr i j ant = concat [[[i,j-1]],(ruta2 arr i (j-1) [i,j])] 
                 |lookDown arr i j ant = concat [[[i,j+1]],(ruta2 arr i (j+1) [i,j])]    
                 |lookLeft arr i j ant  = concat [[[i-1,j]],(ruta2 arr (i-1) j [i,j])]   
                 |lookRight arr i j ant = concat [[[i+1,j]],(ruta2 arr (i+1) j [i,j])]
                                                                        

--Funciones para ver los valores adyacentes a una posicion especifica.
lookUp::([[Char]]->Int->Int->[Int])->Bool
lookUp arr i j ant = if ((arr!!(j-1)!!i)=='0' || (arr!!(j-1)!!i)=='F') && ([i,(j-1)] /= ant) then True else False

lookDown::([[Char]]->Int->Int->[Int])->Bool
lookDown arr i j ant = if ((arr!!(j+1)!!i)=='0' || (arr!!(j+1)!!i)=='F') && ([i,(j+1)] /= ant) then True else False

lookLeft::([[Char]]->Int->Int->[Int])->Bool
lookLeft arr i j ant = if ((arr!!j!!(i-1))=='0' || (arr!!j!!(i-1))=='F') && ([(i-1),j] /= ant) then True else False

lookRight::([[Char]]->Int->Int->[Int])->Bool
lookRight arr i j ant = if ((arr!!j!!(i+1))=='0' || (arr!!j!!(i+1))=='F') && ([(i+1),j] /= ant) then True else False


