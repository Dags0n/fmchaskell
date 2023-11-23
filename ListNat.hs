{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use foldr" #-}
module ListNat where

import Prelude hiding (length, sum, product, (++), reverse, Bool, False, True, odd)
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

product :: ListNat -> Nat
product [] = S O
product (x : xs) = mult x (product xs)

(++) :: ListNat -> ListNat -> ListNat
(++) [] ys = ys
(++) (x : xs) ys = x : (++) xs ys

reverse :: ListNat -> ListNat
reverse [] = []
reverse (x : xs) = (++) (reverse xs) [x]

allEven :: ListNat -> Bool
allEven (x : xs) = if_then_else_ (ev x) (allEven xs) False
allEven [] = True

anyEven :: ListNat -> Bool
anyEven (x : xs) = if_then_else_ (ev x) True (anyEven xs)
anyEven [] = False

allOdd :: ListNat -> Bool
allOdd (x : xs) = if_then_else_ (odd x) (allOdd xs) False
allOdd [] = True

anyOdd :: ListNat -> Bool
anyOdd (x : xs) = if_then_else_ (odd x) True (anyOdd xs)
anyOdd [] = False