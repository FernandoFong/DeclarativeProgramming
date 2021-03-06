module Tarea0 where

data Nat = Cero | S Nat deriving Show

suma :: Nat -> Nat -> Nat
suma Cero n = n
suma (S n) m = S (suma n m)

prod :: Nat -> Nat -> Nat
prod (S Cero) n = n
prod (S n) m = suma (prod n m) m

iguales :: Nat -> Nat -> Bool
iguales Cero Cero = True
iguales (S Cero) Cero = False
iguales Cero (S Cero) = False
iguales (S n) (S m) = iguales n m

h :: Int -> Int -> Int
h n 0 = 1
h n m = if(mod m 2 == 0)
        then h (n*n) (div m 2)
        else n * h n (m - 1)

second :: (a,a) -> a
second (x,y) = y

pyu :: [a] -> (a,a)
pyu [x] = (x,x)
pyu (x:xs) = (x, second (pyu xs))

clonaAux :: Int -> Int ->[Int]
clonaAux n c = if c > 0 then  n:(clonaAux n (c-1)) else []
         
clona :: [Int] -> [Int]
clona [] = []
clona (x:xs) = (clonaAux x x) ++ (clona xs)

agrupa :: [Int] -> [[Int]]
agrupa [x] = [[x]]
agrupa (x:xs) = let zs = agrupa xs in
                  if (head (head zs) == x)
                  then (x:(head zs)):(tail zs)
                  else [x]:zs
                      

frec :: [Int] -> [(Int, Int)]
frec [] = []
frec (x:xs) = let aux = [y | y <- xs, y /= x] in
                (x, (cuenta x (x:xs))):(frec aux)

cuenta :: Int -> [Int] -> Int
cuenta n [] = 0
cuenta n (x:xs) = let total = cuenta n xs in
                    if x == n then total + 1 else total
