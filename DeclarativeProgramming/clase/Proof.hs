module Proof where

data Nat = Zero | S Nat deriving Show

plus :: Nat -> Nat -> Nat
plus Zero n = n
plus (S n) m = S (plus n m)

times :: Nat -> Nat -> Nat
times (S Zero) m = m
times (S n) m = (plus (times n m) m)

toInt :: Nat -> Int
toInt Zero = 0
toInt (S n) = 1 + (toInt n)
