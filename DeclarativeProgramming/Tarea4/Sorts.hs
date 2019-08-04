module Sorts where

--Un arreglo es una función de los enteros en un tipo 'a'; la segunda componente es para conocer su longitud.
data Arreglo a = Arr (Int -> a) Int

arr = Arr f 10 where 
          f n = case n of
                 0 -> 1
                 1 -> 2
                 2 -> 23
                 3 -> 1
                 4 -> 0
                 5 -> 1
                 6 -> 10
                 7 -> 11
                 8 -> 22
                 9 -> 9
                 _ -> error "Array Index Out Of Bounds"
                 
--Para pintar los arreglos de forma bonita
instance (Show a) => Show (Arreglo a) where
      show (Arr f n) =  "{"++ pinta 0 n f where 
                              pinta i n f | n==0 = "}" 
                                          | i==0 = show (f i)++pinta (i+1) n f 
                                          | i==n = "}"
                                          | otherwise = ","++show (f i)++pinta (i+1) n f

--Para obtener el elemento en la i-ésima posición de un arreglo
get::Arreglo a->Int->a
get (Arr f n) i | i>=0 && i < n = f i
                | otherwise = error "Array Index Out Of Bounds"   

--Para sobreescribir el elemento en la i-ésima posición
upd::Arreglo a->Int->a->Arreglo a
upd (Arr f n) i x = Arr (\m -> if m==i then x else f m) n

--Para obtener el tamaño de un arreglo
size::Arreglo a->Int
size (Arr _ n) = n

--Para saber si un elemento pertenece a un arreglo
elemArr::Eq a=>a->Arreglo a->Int
elemArr x arr = busca x 0 arr where
                busca x i arr | i < size arr = if x == get arr i then i else busca x (i+1) arr      
                              | otherwise = -1


--Para obtener la posición del mínimo en un arreglo que está en la posición 'i' en adelante.
minArr::Ord a=>Arreglo a->Int->Int
minArr arr i = buscaMin i (i+1) arr where
               buscaMin m_i j arr | j==size arr = m_i
                                  | otherwise = if get arr m_i < get arr j then buscaMin m_i (j+1) arr else buscaMin j (j+1) arr               

--Intercambia los elementos en la posición 'i' y 'j' de un arreglo.
swap::Arreglo a->Int->Int->Arreglo a
swap arr i j = let xi = get arr i
                   xj = get arr j
                   f' n | n == i = xj
                        | n == j = xi
                        | otherwise = get arr n in
               Arr f' (size arr)

--Ordena un arreglo con selectionSort
selectionSort::Ord a=>Arreglo a->Arreglo a
selectionSort arr = ordena 0 arr where
                    ordena i arr | i<size arr = let m = minArr arr i 
                                                    arr' = swap arr i m in 
                                                ordena (i+1) arr'              
                                 | otherwise = arr

busquedaBinaria :: Ord a => Arreglo a -> a -> Maybe Int
busquedaBinaria arr e = busBinAux arr e 0 ((size arr)-1)

busBinAux :: Ord a => Arreglo a -> a -> Int -> Int-> Maybe Int
busBinAux arr e a b
  | a > b = Nothing
  | get arr (div (a+b)2) == e = Just (div (a+b) 2) 
  | get arr (div (a+b) 2) > e = busBinAux arr e a ((div (a+b) 2)-1)
  | otherwise = busBinAux arr e ((div (a+b) 2)+1) b

maximo :: (Ord a) => Arreglo a -> Int -> Int -> Int
maximo arr i n = let max = if (izq < n) && ((get arr izq) > (get arr i)) then izq else i
                 in if(der < n) && ((get arr der) > (get arr max)) then der else max
                 where izq = 2 * i + 1 
                       der = 2 * i + 2 


heapify :: (Ord a) => Arreglo a -> Int -> Int -> Arreglo a
heapify arr i n = if max' /= i then heapify (swap arr max' i) max' n else arr 
                  where max' = maximo arr i n

creaHeap ::  (Ord a) => Arreglo a -> Int -> Arreglo a
creaHeap arr 0 = heapify arr 0 (size arr)
creaHeap arr i = creaHeap  (heapify arr i (size arr)) (i-1)

auxheapSort :: (Ord a) => Arreglo a -> Int -> Arreglo a
auxheapSort arr i = let swapped = (swap arr i 0) 
                    in if i/=1 then auxheapSort (heapify swapped 0 i) (i-1) else (heapify swapped 0 i)

heapSort :: Ord a => Arreglo a -> Arreglo a
heapSort arr = let heap = creaHeap arr ((size arr) `div` 2) in auxheapSort heap ((size arr) - 1)

ejer1 :: Arreglo Int -> Arreglo Int
ejer1 a@(Arr f n) = heapSort a

pasaArreglo :: [Integer] -> Arreglo Integer
pasaArreglo ls = Arr (\n -> ls!!n) (length ls)

ejer2 :: Arreglo Int -> Arreglo Int -> Int -> Maybe (Int, Int)
ejer2 a b x = let a' = heapSort a
                  b' = heapSort b in auxEjer2 a b x 0

--Dejo este arreglo para pruebas porque el de arriba hace una cosa rara entre Int e Integer. :v
arr1 = Arr (\i -> if i >= 0 && i <= 4 then i*2 else error "Array Index Out of Bounds") 5

auxEjer2 :: Arreglo Int -> Arreglo Int -> Int -> Int -> Maybe (Int,Int)
auxEjer2 a b x pos
  | pos == (size a) = Nothing
  | otherwise = let x' = x - (get a pos)
                    pos_b = busquedaBinaria b x' in
                  case pos_b of
                    Nothing -> auxEjer2 a b x (pos+1)
                    Just i -> Just(pos, i)                  

ejer3 :: Arreglo Int -> Int -> Maybe (Int, Int)
ejer3 a x = let a' = heapSort a
                i = (busquedaBinaria a (get a' 0)) in
              if (get a' 0) + (get a' 0) > x
              then Just (saca i, saca i)
              else auxEjer3 a a' (x - (get a' 0))

saca :: Maybe Int -> Int
saca m = case m of
           Nothing -> -1
           Just x -> x

auxEjer3 :: Arreglo Int -> Arreglo Int -> Int -> Maybe (Int,Int)
auxEjer3 a a' x = let j = busquedaBinaria a' x in
                    case j of
                      Nothing -> Just (0, saca (busquedaBinaria a (get a' ((size a)-1))))
                      Just x -> Just(0, saca (busquedaBinaria a (get a' x)))

ejer4 :: Arreglo String -> Arreglo String
ejer4 arr = let tam = cuentaArr arr 0 (0,0,0) in
              case tam of
                (x,y,z) -> Arr (\i -> if i >= 0 && i <= x-1
                                          then "Verde"
                                          else if i >= x && i <= x+y-1
                                            then "Rojo"
                                            else "Azul") (x+y+z)

cuentaArr :: Arreglo String -> Int -> (Int, Int, Int) -> (Int, Int, Int)
cuentaArr arr i tern@(x,y,z)
  |i < size arr = let e = get arr i in
                    case e of
                      "Verde" -> cuentaArr arr (i+1) (x+1, y, z)
                      "Rojo" -> cuentaArr arr (i+1) (x, y+1, z)
                      _ -> cuentaArr arr (i+1) (x, y, z+1)
  |otherwise = tern

prueba = Arr f 10 where
          f n = case n of
                 0 -> "Verde"
                 1 -> "Azul"
                 2 -> "Rojo"
                 3 -> "Azul"
                 4 -> "Rojo"
                 5 -> "Verde"
                 6 -> "Verde"
                 7 -> "Rojo"
                 8 -> "Azul"
                 9 -> "Azul"
                 _ -> error "fuera de índice"

ejer5 :: [Int]  -> [Int] -> [Int]
ejer5 [a,b] [c,d] = let a' = a*c
                        b' = b*d
                        c' = (a+b)*(c+d) in
                      [a',c'-a'-b',b']
