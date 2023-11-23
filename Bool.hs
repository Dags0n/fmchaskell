{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use camelCase" #-}
module Bool where

import Prelude hiding (True, False, Bool)
import Nat

data Bool = False | True
    deriving ( Eq , Show )


if_then_else_ :: Bool -> a -> a -> a
if_then_else_ True n _ = n
if_then_else_ False _ m = m

leq :: Nat -> Nat -> Bool
leq (S n) (S m) = leq n m
leq O _ = True
leq _ _ = False