module Curve.BinaryWeierstrass.SECT163K1
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

-- | SECT163K1 curve
data SECT163K1

-- | Field of SECT163K1 curve
type F2m = BinaryField 0x800000000000000000000000000000000000000c9

-- | SECT163K1 curve is a binary Weierstrass curve
instance BWCurve SECT163K1 F2m where
  a_ = const _a 
  {-# INLINE a_ #-}
  b_ = const _b
  {-# INLINE b_ #-}
  g_ = _g
  {-# INLINE g_ #-}

-- | Point of SECT163K1 curve
type P = BWPoint SECT163K1 F2m

-------------------------------------------------------------------------------
-- Parameters
-------------------------------------------------------------------------------

-- | Coefficient @A@ of SECT163K1 curve
_a :: F2m
_a = 1
{-# INLINE _a #-}

-- | Coefficient @B@ of SECT163K1 curve
_b :: F2m
_b = 1
{-# INLINE _b #-}

-- | Polynomial of SECT163K1 curve
_f :: Integer
_f = 0x800000000000000000000000000000000000000c9
{-# INLINE _f #-}

-- | Generator of SECT163K1 curve
_g :: P
_g = A
     0x02fe13c0537bbc11acaa07d793de4e6d5e5c94eee8
     0x0289070fb05d38ff58321f2e800536d538ccdaa3d9
{-# INLINE _g #-}

-- | Cofactor of SECT163K1 curve
_h :: Integer
_h = 2
{-# INLINE _h #-}

-- | Order of SECT163K1 curve
_n :: Integer
_n = 0x04000000000000000000020108a2e0cc0d99f8a5ef
{-# INLINE _n #-}
