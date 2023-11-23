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