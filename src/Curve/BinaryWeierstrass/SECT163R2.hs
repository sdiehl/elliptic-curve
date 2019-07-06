module Curve.BinaryWeierstrass.SECT163R2
  -- | Types
  ( F2m
  , P
  -- | Parameters
  , _a
  , _b
  , _f
  , _g
  , _h
  , _n
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
  a_ = const _a 
  {-# INLINE a_ #-}
  b_ = const _b
  {-# INLINE b_ #-}
  g_ = _g
  {-# INLINE g_ #-}

-- | Point of SECT163R2 curve
type P = BWPoint SECT163R2 F2m

-------------------------------------------------------------------------------
-- Parameters
-------------------------------------------------------------------------------

-- | Coefficient @A@ of SECT163R2 curve
_a :: F2m
_a = 1
{-# INLINE _a #-}

-- | Coefficient @B@ of SECT163R2 curve
_b :: F2m
_b = 0x020a601907b8c953ca1481eb10512f78744a3205fd
{-# INLINE _b #-}

-- | Polynomial of SECT163R2 curve
_f = split (witness :: F2m)
{-# INLINE _f #-}

-- | Generator of SECT163R2 curve
_g :: P
_g = A 0x03f0eba16286a2d57ea0991168d4994637e8343e36
       0x00d51fbc6c71a0094fa2cdd545b11c5c0c797324f1
{-# INLINE _g #-}

-- | Cofactor of SECT163R2 curve
_h :: Integer
_h = 2
{-# INLINE _h #-}

-- | Order of SECT163R2 curve
_n :: Integer
_n = 0x040000000000000000000292fe77e70c12a4234c33
{-# INLINE _n #-}
