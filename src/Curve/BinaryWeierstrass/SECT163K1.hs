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

import ExtensionField (IrreducibleMonic(..), x)

import Curve.BinaryWeierstrass (BWCurve(..), BWPoint, F2, Fm, Point(..))

-------------------------------------------------------------------------------
-- Types
-------------------------------------------------------------------------------

-- | SECT163K1 curve
data SECT163K1

-- | Field of SECT163K1 curve
data FX
instance IrreducibleMonic F2 FX where
  split _ = x ^ (163 :: Int) + x ^ (7 :: Int) + x ^ (6 :: Int) + x ^ (3 :: Int) + 1
type F2m = Fm FX

-- | SECT163K1 curve is a binary Weierstrass curve
instance BWCurve SECT163K1 FX where
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
_f = split (witness :: F2m)
{-# INLINE _f #-}

-- | Generator of SECT163K1 curve
_g :: P
_g = A 0x02fe13c0537bbc11acaa07d793de4e6d5e5c94eee8
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
