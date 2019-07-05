module Curve.BinaryWeierstrass.SECT163R1
  -- | Imports
  ( BWCurve(..)
  , BWPoint
  , F2
  , Fm
  , Point(..)
  -- | Types
  , F2m
  , P
  ) where

import Protolude

import ExtensionField (IrreducibleMonic(..), x)

import Curve.BinaryWeierstrass (BWCurve(..), BWPoint, F2, Fm, Point(..))

-------------------------------------------------------------------------------
-- Types
-------------------------------------------------------------------------------

-- | SECT163R1 curve
data SECT163R1

-- | Field of SECT163R1 curve
data FX
instance IrreducibleMonic F2 FX where
  split _ = x ^ (163 :: Int) + x ^ (7 :: Int) + x ^ (6 :: Int) + x ^ (3 :: Int) + 1
type F2m = Fm FX

-- | SECT163R1 curve is a binary Weierstrass curve
instance BWCurve SECT163R1 FX where
  _a _ = 0x07b6882caaefa84f9554ff8428bd88e246d2782ae2
  {-# INLINE _a #-}
  _b _ = 0x0713612dcddcb40aab946bda29ca91f73af958afd9
  {-# INLINE _b #-}
  _f _ = witness :: FX
  {-# INLINE _f #-}
  _g   = A 0x0369979697ab43897789566789567f787a7876a654
           0x00435edb42efafb2989d51fefce3c80988f41ff883
  {-# INLINE _g #-}
  _h _ = 2
  {-# INLINE _h #-}
  _n _ = 0x03ffffffffffffffffffff48aab689c29ca710279b
  {-# INLINE _n #-}

-- | Point of SECT163R1 curve
type P = BWPoint SECT163R1 F2m
