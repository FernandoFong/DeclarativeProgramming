module Remember where

data Nat = Zero
         | S Nat deriving Show

toInt :: Nat -> Int
toInt Zero = 0
toInt (S n) = 1 + toInt n

add :: Nat -> Nat -> Nat
add n Zero = n
add (S n) (S m) = add (S (S n)) m

times :: Nat -> Nat -> Nat
times n (S Zero) = n
times n (S m) = times (add n n) m

fact :: Nat -> Nat
fact Zero = (S Zero)
fact (S n) = times (S n) (fact n)
