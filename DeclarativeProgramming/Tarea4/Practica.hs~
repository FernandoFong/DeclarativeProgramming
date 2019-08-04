module Practica where

import System.Random
import Control.Monad
import Sorts

esPrimo :: Int -> Bool
esPrimo n | n < 2 = False
          | n == 2 = True
          | even n = False
          | otherwise = null [ m | m <- imparesHasta n, mod n m == 0]  
          where imparesHasta n = [m | m<-[3,5..(div n 2)]]

goldbach_list :: Int -> [(Int, Int, Int)]
goldbach_list n = [(a,b,c) | a <- [1..n], b <- [a..n], c <- [b..n], esPrimo a, esPrimo b, esPrimo c, a+b+c == n]

goldbach_do :: Int -> [(Int,Int,Int)]
goldbach_do n =
  do
    a <- [1..n];
    b <- [a..n];
    c <- [b..n];
    guard $ esPrimo a;
    guard $ esPrimo b;
    guard $ esPrimo c;
    guard $ a+b+c == n;
    return (a,b,c)

goldbach_bind :: Int -> [(Int,Int,Int)]
goldbach_bind n = [1..n] >>= \a ->
                  [a..n] >>= \b ->
                  [b..n] >>= \c ->
                  guard (a+b+c == n && esPrimo a && esPrimo b && esPrimo c) >>
                  return (a,b,c)

instance Functor Arreglo where
  fmap g (Arr f n) = (Arr (g.f) n)

newtype State s a = ST (s ->(a,s))

runState :: State s a -> s ->(a,s)
runState (ST st) s = st s

instance Functor (State s) where
  fmap g st = ST (\i -> let (a,s) = runState st i
                        in (g a, s))

instance Applicative (State s) where
  pure a = return a
  (<*>) sf sv = ST (\st -> let (f, st1) = runState sf st
                               (a, st2) = runState sv st in
                             (f a, st2))

instance Monad (State s) where
  return x = ST (\s -> (x,s))
  st >>= f = ST (\s -> let (a,s1) = runState st s in
                         runState (f a) s1)

auxArrState :: Ord a => Arreglo a -> State (a, Int, Int) a
auxArrState arr =
  do
    (m,p,i) <- getState;
    if i == (size arr) then return m else
      let e = (get arr i) in
        if e < m
        then do
          updState (e, i, (i+1))
          auxArrState arr
        else do
          updState (m, p, (i+1))
          auxArrState arr

minArrState :: Ord a => Arreglo a -> Int -> Int
minArrState arr@(Arr f n) i =
  let min = runState (auxArrState arr) (get arr i,i,i) in
    case min of
      (val, (val', pos, len)) -> pos

selectionSortState :: Ord a => Arreglo a -> Arreglo a
selectionSortState arr = 
  let ord = runState (auxSelState arr) (arr, 0) in
    case ord of
      (size, (arr_st, pos)) -> arr_st

auxSelState :: Ord a => Arreglo a -> State (Arreglo a, Int) Int
auxSelState arr = 
    do
      (arr', pos) <- getState;
      if pos == (size arr) then return pos else
        let min = minArrState arr' pos in
          do
            updState ((swap arr' min pos), (pos+1));
            auxSelState arr;

getState = ST $ \s -> (s,s)

updState s' =  ST $ \_ -> ((),s')

length1 :: [Int] -> State [Int] Int
length1 [] = return 0
length1 (x:xs) = let past = runState (length1 xs) xs in
                   ST (\st -> (1+(fst past), (x:xs)))

--El tipo para hacer cómputos CPS
newtype Continuation r a = Cont ((a->r)->r)

--Para correr un cómputo CPS.
runCont::Continuation r a->(a->r)->r
runCont (Cont cont) k = cont k

instance Functor (Continuation r) where
  fmap f c = error "Te toca"
                      
instance Applicative (Continuation r) where
   pure =  return
   (<*>) mf mx = error "Te toca"

instance Monad (Continuation r) where
   return x = Cont $ \k -> k x
   ka >>= f = Cont $ \k -> runCont ka $ \a -> runCont (f a) k

lengthcps :: [a] -> Continuation r Int
lengthcps [] = return 0
lengthcps (x:xs) =
  do
    m <- (lengthcps xs);
    return $ m+1

takecps :: Int -> [a] -> Continuation r [a]
takecps 0 ls = return []
takecps n [] = return []
takecps n (x:xs) =
  do
    m <- takecps (n-1) xs;
    return (x:m)

dropcps :: Int -> [a] -> Continuation r [a]
dropcps 0 ls = return ls
dropcps n [] = return []
dropcps n (x:xs) =
  do
    m <- dropcps (n-1) xs;
    return m
    
platillos = ["Pollo asado", "Carne asada", "Pescado a la plancha",
             "Pollo encacahuatado","Carne encebollada","Pollo frito",
             "Pollo en salsa verde","Enchiladas","Torta de chorizo",
             "Pescado empapelado","Sushi","Carne en pasilla",
             "Tacos al pastor","Atún con verdura y mayonesa",
             "Chorizo argentino asado","Cortes de carne","Ensalada",
             "Pollo asado con arroz","Pescado empanizado",
             "Coctel de camarón"]
dieta =
  do
    putStrLn "Tu dieta de esta semana va a ser";
    i <- randomRIO (0,19);
    putStrLn $ "\tLunes: "++platillos !! i;
    i <- randomRIO (0,19);
    putStrLn $ "\tMartes: "++platillos !! i;
    i <- randomRIO (0,19);
    putStrLn $ "\tMiercoles: "++platillos !! i;
    i <- randomRIO (0,19);
    putStrLn $ "\tJueves: "++platillos !! i;
    i <- randomRIO (0,19);
    putStrLn $ "\tViernes: "++platillos !! i;
    i <- randomRIO (0,19);
    putStrLn $ "\tSábado: "++platillos !! i;
    i <- randomRIO (0,19);
    putStrLn $ "\tDomingo: "++platillos !! i;

mayusculas = ['A','B','C','D','E','F','G','H','I','J','K','L','M','N','O','P','Q','R','S','T','U','V','W','X','Y','Z']

numeros = ['0','1','2','3','4','5','6','7','8','9']

hayMayusculas :: [Char] -> Bool
hayMayusculas [x] = elem x mayusculas
hayMayusculas (x:xs) = (elem x mayusculas) || (hayMayusculas xs)

hayNumeros :: [Char] -> Bool
hayNumeros [x] = elem x numeros
hayNumeros (x:xs) = (elem x numeros) || (hayNumeros xs)

getPass2 =
  do
    putStrLn "Escribe una contraseña";
    pass <- getLine;
    if  mensajePass (pass) /= pass
      then
      do
        putStrLn (mensajePass (pass))
        getPass2;
      else
      do
        return pass;

mensajePass :: [Char] -> [Char]
mensajePass pass
  | length pass < 8 = "Tamaño incorrecto, debe medir al menos 8"
  | not (hayMayusculas pass) = "Debe incluir una mayuscula"
  | not (hayNumeros pass) = "Debe incluir un numero"
  |  otherwise =  pass
