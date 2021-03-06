module Tarea2 where
{-Facultad de Ciencias UNAM - Programación Declarativa 2018-2 
      Profesor: C. Moisés Vázquez Reyes
      Ayudante: Enrique Antonio Bernal Cedillo
-}


--Los átomos son cadenas.
type At = String
--Fórmulas de la lógica proposicional en forma normal negativa.
data F = Var At | Neg At | Conj F F | Disy F F deriving Eq 

--Para pintar fórmulas de forma especial.
instance Show F where
   show f = case f of
             Var p -> p
             Neg p -> "¬"++p
             Conj f1 f2 -> case f1 of
                            Var _ -> show f1 ++ "∧" ++ case f2 of
                                                        Var _ -> show f2
                                                        Neg _ -> show f2
                                                        _ -> "("++show f2++")"
                            Neg _ -> show f1 ++ "∧" ++ case f2 of
                                                        Var _ -> show f2
                                                        Neg _ -> show f2
                                                        _ -> "("++show f2++")"
                            _ -> "("++show f1 ++ ")∧" ++ case f2 of
                                                        Var _ -> show f2
                                                        Neg _ -> show f2
                                                        _ -> "("++show f2++")"  
             Disy f1 f2 -> case f1 of
                            Var _ -> show f1 ++ "∨" ++ case f2 of
                                                        Var _ -> show f2
                                                        Neg _ -> show f2
                                                        _ -> "("++show f2++")"
                            Neg _ -> show f1 ++ "∨" ++ case f2 of
                                                        Var _ -> show f2
                                                        Neg _ -> show f2
                                                        _ -> "("++show f2++")"
                            _ -> "("++show f1 ++ ")∨" ++ case f2 of
                                                        Var _ -> show f2
                                                        Neg _ -> show f2
                                                        _ -> "("++show f2++")"                                                                                                      

--Para negar fórmulas en general.
neg::F->F
neg f = case f of
         Var p -> Neg p
         Neg p -> Var p
         Conj f1 f2 -> Disy (neg f1) (neg f2)
         Disy f1 f2 -> Conj (neg f1) (neg f2)


--La implicación es un caso particular de la disyunción.
imp::F->F->F
imp f1 f2 = Disy (neg f1) f2

{-=============================================================-}
--Una literal es una fórmula atómica o la negación de una fórmula atómica.
--Se considera que una 'Literal' únicamente es de la forma 'Var _' o 'Neg _'.
type Literal = F
--Una cláusula es una literal o una disyunción de literales.
--La lista [l1,l2,..,lk] significa (l1 ⋁ l2 ⋁..⋁ lk)
type Clausula = [Literal]

--Transforma una fórmula a FNC.
fnc :: F -> F
fnc v@(Var v1) = v
fnc n@(Neg n1) = n
fnc (Disy x (Conj y z)) = (Conj (fnc (Disy x y)) (fnc (Disy x z)))
fnc (Disy (Conj x y) z) = (Conj (fnc (Disy x z)) (fnc (Disy y z)))
fnc (Conj x y) = (Conj (fnc x) (fnc y))
fnc (Disy x y) = let xf = fnc x
                     yf = fnc y in
                   case (xf, yf) of
                     (Conj x' x'', _) -> fnc (Disy (Conj x' x'') yf)
                     (_, Conj y' y'') -> fnc (Disy xf (Conj y' y''))
                     (_,_) -> Disy xf yf
  
--Obtiene las cláusulas de una fórmula.
clausulas :: F -> [Clausula]
clausulas v@(Var p) = [[v]]
clausulas n@(Neg p) = [[n]]
clausulas (Disy p q) = let cp = concat (clausulas p)
                           cq = concat (clausulas q) in
                         [[x | x <- (cp ++ cq)]]
clausulas (Conj p q) = (clausulas p) ++ (clausulas q)
                         
  

--Realiza el algoritmo DPLL y pinta en pantalla el árbol generado por la ejecución,
--y en cada nivel se indica la operación realizada.

data BaseDPLL = Vacia
              | CU [Clausula] Clausula [Clausula] BaseDPLL
              | Fallo [Clausula]
              | LP [Clausula] Literal [Clausula] BaseDPLL
              | Desc [Clausula] F [Clausula] [Clausula] BaseDPLL BaseDPLL

instance Show BaseDPLL where
  show t = pinta t 0 where
           pinta t n = case t of
                         Vacia -> (replicate n ' ') ++ "□"
                         Fallo ls-> (replicate n ' ') ++ "Error! Contradiccion " ++ show ls ++ "\nx"
                         CU ant c ls sig -> (replicate n ' ')++show ant++ "\nRegla Cláusula Unitaria "++show c ++ " "++show ls++"\n"++pinta sig n
                         LP ant pura ls sig -> (replicate n ' ') ++show ant++"\nRegla Literal Pura "++ show pura++" " ++ show ls ++ "\n" ++ pinta sig n
                         Desc ant c res1 res2 n1 n2 -> (replicate n ' ') ++ show ant++ "\nSup. que "++show c++ " vale true:\n"++show res1++"\n"
                                                       ++ pinta n1 (n+3) ++ "\n"
                                                       ++ "Sup. que "++show c++ " vale false:\n"++show res2++"\n" ++ pinta n2 (n+3)

dpll :: [Clausula] -> BaseDPLL
dpll lls | soloVacias lls = Fallo lls
         | hayClausulaUnit lls = let c = clausulaUnit lls in
                                   CU lls c lls (dpll (auxCU (head c) lls))
         | hayLitPura lls = let pura = litPura lls in
                                case pura of
                                  Nothing -> error "Caso tonto, nunca cae aquí"
                                  Just p -> let lista = [x | x <- lls, not (elem p x)] in
                                              LP lls p lista (dpll lista)
         | lls == [] = Vacia
         | otherwise = let sup = head (head lls)
                           res1 = myMap lls sup
                           res2 = myMap lls (neg sup) in
                         Desc lls sup res1 res2 (dpll res1) (dpll res2)

hayClausulaUnit :: [Clausula] -> Bool
hayClausulaUnit lls = [x | x <- lls, length x == 1] /= []

clausulaUnit :: [Clausula] -> Clausula
clausulaUnit lls = head [x | x <- lls, length x == 1]

hayLitPura :: [Clausula] -> Bool
hayLitPura lls = auxLP (concat lls)

auxLP :: Clausula -> Bool
auxLP [] = False
auxLP (x:xs) = let n = neg x in
                 not (elem n xs) || auxLP [y | y <- xs, y /= n, y /= x]

litPura :: [Clausula] -> Maybe Literal
litPura lls = auxLit (concat lls)

auxLit :: Clausula -> Maybe Literal
auxLit [] = Nothing
auxLit (x:xs) = let n = neg x in
                  if not (elem n xs) then Just x else auxLit [y | y <- xs, y /= n, y /= x]

auxDesc :: Clausula -> Literal -> Clausula
auxDesc ls e = let n = neg e in
                 if elem e ls then [] else [x | x <- ls, x /= n]

myMap :: [Clausula] -> Literal -> [Clausula]
myMap [] e = []
myMap (x:xs) e = (auxDesc x e):(myMap xs e)

soloVacias :: [Clausula] -> Bool
soloVacias [] = False
soloVacias [[]] = True
soloVacias (x:xs) = x == [] && soloVacias xs

auxCU :: Literal -> [Clausula] -> [Clausula]
auxCU e [] = []
auxCU e (x:xs) = let n = neg e in
                   if elem e x
                   then auxCU e xs
                   else
                     if elem n x
                     then [y | y <- x, y /= n]:(auxCU e xs)
                     else x:(auxCU e xs)

ejer1_1 = dpll $ concat $ map clausulas $ [Disy (Var "a") (Var "b"),imp (neg $ Var "c") (neg $ Var "a")]++[neg $ imp (Var "b") (neg $ Var "c")]

ejer1_2 = dpll $ concat $ map clausulas $ [(Disy (imp (Var "p") (Var "r")) (Conj (Neg "s") (Var "p"))),imp (Var "s") (neg (Conj (Var "p") (Var "r")))]++[(Disy (Var "r") (Neg "s"))]

ejer1_3 = dpll $ concat $ map clausulas $ [(Disy (imp (Var "s") (Var "p")) (imp (Var "t") (Var "q")))]++[neg (Disy (imp (Var "s") (Var "p")) (imp (Var "t") (Var "q")))]

ejer1_4 = dpll $ concat $ map clausulas $ [(Conj (Var "p") (Var "q")), (Conj (Var "r") (Neg "s")), (imp (Var "q") (imp (Var "p") (Var "t"))), (imp (Var "t") (imp (Var "r") (Disy (Var "s") (Var "w"))))]++[neg (Var "w")]

a = imp (Var "llueve") (Disy (Var "Primavera") (Var "Invierno"))
b = imp (Var "ríos") (Var "llueve")
c = imp (Var "Primavera") (Var "Usar gorros azules")
d = (Conj (Neg "Usar gorros azules") (Var "ríos"))
conc = Var "Primavera"

ejer2_a = dpll $ concat $ map clausulas $ [a, b, c, d]++[neg conc]

e = Disy (Disy (Var "Maluma") (Var "C-G")) (Var "Kalimba")
f = imp (Conj (Var "Maluma") (Neg "Kalimba")) (Var "C-G")
g = (Disy (Conj (Var "Maluma") (Var "Kalimba")) (Conj (Neg "Maluma") (Neg "Kalimba")))
h = imp (Var "C-G") (Var "Maluma")

ejer2_b = dpll $ concat $ map clausulas $ [e,f,g,h]




