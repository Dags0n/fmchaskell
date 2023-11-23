{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use foldr" #-}
module ListNat where

import Prelude hiding (length, sum)
import Nat
import Bool

type ListNat = [Nat]

length :: ListNat -> Nat
length [] = O
length (x : xs) = S (length xs)

-- elem :: Nat -> ListNat -> Bool

sumLN :: ListNat -> Nat
sumLN [] = O
sumLN (x : xs) = sum x (sumLN xs)