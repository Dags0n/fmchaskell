module Nat where

import Prelude hiding (sum, pred, min, max)

data Nat = O | S Nat
    deriving ( Eq , Show )


sum :: Nat -> Nat -> Nat
sum n O = n
sum n (S m) = S (sum n m)

mult :: Nat -> Nat -> Nat
mult n O = O
mult n (S m) = sum n (mult n m)

pow :: Nat -> Nat -> Nat
pow n O = S O
pow n (S m) = mult n (pow n m)

double :: Nat -> Nat
double O = O
double (S n) = S (S (double n))

pred :: Nat -> Nat
pred O = O
pred (S n) = n

fact :: Nat -> Nat
fact O = S O
fact (S n) = mult (S n) (fact n)

fib :: Nat -> Nat
fib O = O
fib (S O) = S O
fib (S (S n)) = sum (fib (S n)) (fib n)

min :: Nat -> Nat -> Nat
min (S n) (S m) = S (min m n)
min _ _ = O

max :: Nat -> Nat -> Nat
max O m = m
max n O = n
max (S n) (S m) = S (max n m)

-- div :: (Nat, Nat) -> (Nat, Nat)

-- quot :: (Nat, Nat) -> Nat

-- rem ::(Nat, Nat) -> Nat

-- gcd :: (Nat, Nat) -> Nat

-- lcm :: (Nat, Nat) -> Nat

monus :: Nat -> Nat -> Nat
monus (S n) (S m) = monus n m
monus n O = n
monus O _ = O