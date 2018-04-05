------------------------------------------------------------------------
--                                                                    --
-- Declarative Programming                                            --
--                                                                    --
-- Optative subject at Faculty of Sciences, UNAM                      --
--                                                                    --
-- Class from Feb 16, 2018                                            --
--                                                                    --
-- Author Fernando Fong                                               --
--                                                                    --
------------------------------------------------------------------------

module SAT where

type At = String
  
type State = [At]
  
data Form = Var At | Neg At | Conj Form Form | Disy Form Form
          deriving (Eq)

instance Show Form where
  show (Var p) = p
  show (Neg p) = "¬"++ p
  show (Conj f1 f2) = (show f1) ++ "^" ++ (show f2)
  show (Disy f1 f2) = (show f1) ++ "v" ++ (show f2)


neg :: Form -> Form
neg fi = case fi of
           Var p -> Neg p
           Neg p -> Var p
           Conj f1 f2 -> Disy (neg f1) (neg f2)
           Disy f1 f2 -> Conj (neg f1) (neg f2)

potencia :: [a] -> [[a]]
potencia [] = [[]]
potencia (x:xs) = let zs = potencia xs in [x:ys | ys <- zs] ++  zs

eval :: Form -> State -> Bool
eval f s = case f of
             Var p -> (elem p s)
             Neg p -> not(elem p s)
             Conj f1 f2 -> (eval f1 s) && (eval f2 s)
             Disy f1 f2 -> (eval f1 s) || (eval f2 s)

sat :: Form -> Bool
sat f = or [eval f s |s <- potencia (vars f)] where
  vars f = quitaRep $ case f of
                        Var p -> [p]
                        Neg p -> [p]
                        Conj f1 f2 -> vars f1 ++ vars f2
                        Disy f1 f2 -> vars f1 ++ vars f2


quitaRep :: (Eq a) => [a] -> [a]
quitaRep [] = []
quitaRep (x:xs) = x:[y | y <- (quitaRep xs), y /= x]
  
