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

import BinaryField (BinaryField)

import Curve.BinaryWeierstrass (BWCurve(..), BWPoint, Point(..))

-------------------------------------------------------------------------------
-- Types
-------------------------------------------------------------------------------

-- | SECT163R2 curve
data SECT163R2

-- | Field of SECT163R2 curve
type F2m = BinaryField 0x800000000000000000000000000000000000000c9

-- | SECT163R2 curve is a binary Weierstrass curve
instance BWCurve SECT163R2 0x800000000000000000000000000000000000000c9 where
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
_f :: Integer
_f = 0x800000000000000000000000000000000000000c9
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
