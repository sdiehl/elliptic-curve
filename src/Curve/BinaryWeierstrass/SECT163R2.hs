module Curve.BinaryWeierstrass.SECT163R2
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

-- | SECT163R2 curve
data SECT163R2

-- | Field of SECT163R2 curve
data FX
instance IrreducibleMonic F2 FX where
  split _ = x ^ (163 :: Int) + x ^ (7 :: Int) + x ^ (6 :: Int) + x ^ (3 :: Int) + 1
type F2m = Fm FX

-- | SECT163R2 curve is a binary Weierstrass curve
instance BWCurve SECT163R2 FX where
  _a _ = 1
  {-# INLINE _a #-}
  _b _ = 0x020a601907b8c953ca1481eb10512f78744a3205fd
  {-# INLINE _b #-}
  _f _ = witness :: FX
  {-# INLINE _f #-}
  _g   = A 0x03f0eba16286a2d57ea0991168d4994637e8343e36
           0x00d51fbc6c71a0094fa2cdd545b11c5c0c797324f1
  {-# INLINE _g #-}
  _h _ = 2
  {-# INLINE _h #-}
  _n _ = 0x040000000000000000000292fe77e70c12a4234c33
  {-# INLINE _n #-}

-- | Point of SECT163R2 curve
type P = BWPoint SECT163R2 F2m
