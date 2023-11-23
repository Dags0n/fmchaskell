module Nat where

import Prelude hiding (sum, pred)

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