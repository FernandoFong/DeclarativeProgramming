{-Facultad de Ciencias UNAM - Programación Declarativa 2018-2 
      Profesor: C. Moisés Vázquez Reyes
      Ayudante: Enrique Antonio Bernal Cedillo
-}

module Unificacion where

infixr :-> {- Así, el poderador ':->' asocia a la derecha. -}
type Nombre = String

-- Categoría de tipos.
data Tipo = TNat | TBool | X Nombre | Tipo :-> Tipo deriving Eq


instance Show Tipo where
     show t = case t of
            TNat -> "ℕ"
            TBool -> "𝔹"
            X name -> name 
            TNat:->TNat -> "ℕ" ++"->"++"ℕ"
            TNat:->TBool -> "ℕ" ++"->"++"𝔹"
            TNat:->(X name) -> "ℕ"++"->"++name
            TNat:->(t1:->t2) -> "ℕ"++"->("++show t1++"->"++show t2++")"
            TBool:->TBool -> "𝔹" ++"->"++"𝔹"
            TBool:->TNat -> "𝔹" ++"->"++"ℕ"
            TBool:->(X name) -> "Bool"++"->"++name
            TBool:->(t1:->t2) -> "𝔹"++"->("++show t1++"->"++show t2++")"
            (X name):->TNat -> name++"->"++"ℕ"
            (X name):->TBool -> name++"->"++"𝔹"
            (X name1):->(X name2) -> name1++"->"++name2
            (X name):->(t1:->t2) -> name++"->("++show t1++"->"++show t2++")"
            (t1:->t2):->TNat -> "("++show t1++"->"++show t2++")"++"->"++"ℕ"
            (t1:->t2):->TBool -> "("++show t1++"->"++show t2++")"++"->"++"𝔹"
            (t1:->t2):->(X name) -> "("++show t1++"->"++show t2++")"++"->"++name
            (t1:->t2):->(t3:->t4) -> "("++show t1++"->"++show t2++")"++"->("++show t3++"->"++show t4++")"


--Una sustitución es un conjunto de la forma [(xi, Ti)]
type Sust = [(Nombre, Tipo)]


--Elimina sustituciones de la forma [X:=X] en una sustitución.
simpSust :: Sust -> Sust
simpSust [] = []
simpSust ((x,(X y)):xs) = if x == y
                          then simpSust xs
                          else (x, X y) : (simpSust xs)
simpSust (x:xs) = x:(simpSust xs)

--Realiza la composición de dos sustituciones.
compSust :: Sust -> Sust -> Sust
compSust s1 s2 = simpSust(s1 ++ s2)


--Aplica una sustitución a un tipo.
apSustT :: Tipo -> Sust -> Tipo
apSustT t [] = t
apSustT t s@(x:xs) = let zt = sust t x in apSustT zt xs
                       
--Función auxiliar que nos hace las sustituciones de tipo.
sust :: Tipo -> (Nombre, Tipo) -> Tipo
sust t@(X x) (y, e) = if x == y then e else t
sust (t1:->t2) e'@(v, e) = (sust t1 e') :-> (sust t2 e')
sust t@TNat s = t
sust t@TBool s = t

--Unifica dos tipos.
unifica :: Tipo -> Tipo -> [Sust]
unifica TNat t2 = []
unifica TBool t2 = []
unifica (X x) r@(X y) = if x == y then [] else [[(x, r)]]
unifica (X x) t = if not (elem x (vars t)) then [[(x, t)]] else []
unifica (t1 :-> t2) (t3 :-> t4) = (unifica t1 t3) ++ (unifica t2 t4)

vars :: Tipo -> [Nombre]
vars (X x) = [x]
vars (t1 :-> t2) = (vars t1) ++ (vars t2)
vars _ = []

--Unifica una lista de tipos.
unificaConj :: [(Tipo,Tipo)] -> [Sust]
unificaConj [] = [[]]
unificaConj ((t1,t2):ts) = [compSust s1 s2 | s1 <- unifica t1 t2, s2 <- unificaConj [(apSustT (fst t) s1,apSustT (snd t) s1) | t <- ts]]



