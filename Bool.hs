{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use camelCase" #-}
module Bool where

import Prelude hiding (True, False, Bool, not)
import Nat

data Bool = False | True
    deriving ( Eq , Show )


if_then_else_ :: Bool -> a -> a -> a
if_then_else_ True n _ = n
if_then_else_ False _ m = m

not :: Bool -> Bool
not True = False
not False = True

leq :: Nat -> Nat -> Bool
leq (S n) (S m) = leq n m
leq O _ = True
leq _ _ = False

ev :: Nat -> Bool
ev (S (S n)) = ev n
ev O = True
ev _ = False

odd :: Nat -> Bool
odd n = not (ev n)

isMul3 :: Nat -> Bool
isMul3 (S (S n)) = isMul3 n 
isMul3 O = True
isMul3 _ = False

-- divides :: Nat -> Nat -> Bool

isZero :: Nat -> Bool
isZero O = True
isZero _ = False