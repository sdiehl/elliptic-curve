module Curve.BinaryWeierstrass.SECT233K1
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

-- | SECT233K1 curve
data SECT233K1

-- | Field of SECT233K1 curve
data FX
instance IrreducibleMonic F2 FX where
  split _ = x ^ (233 :: Int) + x ^ (74 :: Int) + 1
type F2m = Fm FX

-- | SECT233K1 curve is a binary Weierstrass curve
instance BWCurve SECT233K1 FX where
  a_ = const _a 
  {-# INLINE a_ #-}
  b_ = const _b
  {-# INLINE b_ #-}
  g_ = _g
  {-# INLINE g_ #-}

-- | Point of SECT233K1 curve
type P = BWPoint SECT233K1 F2m

-------------------------------------------------------------------------------
-- Parameters
-------------------------------------------------------------------------------

-- | Coefficient @A@ of SECT233K1 curve
_a :: F2m
_a = 0
{-# INLINE _a #-}

-- | Coefficient @B@ of SECT233K1 curve
_b :: F2m
_b = 1
{-# INLINE _b #-}

-- | Polynomial of SECT233K1 curve
_f = split (witness :: F2m)
{-# INLINE _f #-}

-- | Generator of SECT233K1 curve
_g :: P
_g = A 0x017232ba853a7e731af129f22ff4149563a419c26bf50a4c9d6eefad6126
       0x01db537dece819b7f70f555a67c427a8cd9bf18aeb9b56e0c11056fae6a3
{-# INLINE _g #-}

-- | Cofactor of SECT233K1 curve
_h :: Integer
_h = 4
{-# INLINE _h #-}

-- | Order of SECT233K1 curve
_n :: Integer
_n = 0x8000000000000000000000000000069d5bb915bcd46efb1ad5f173abdf
{-# INLINE _n #-}
