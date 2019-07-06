module Curve.BinaryWeierstrass.SECT239K1
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

-- | SECT239K1 curve
data SECT239K1

-- | Field of SECT239K1 curve
data FX
instance IrreducibleMonic F2 FX where
  split _ = x ^ (239 :: Int) + x ^ (158 :: Int) + 1
type F2m = Fm FX

-- | SECT239K1 curve is a binary Weierstrass curve
instance BWCurve SECT239K1 FX where
  a_ = const _a 
  {-# INLINE a_ #-}
  b_ = const _b
  {-# INLINE b_ #-}
  g_ = _g
  {-# INLINE g_ #-}

-- | Point of SECT239K1 curve
type P = BWPoint SECT239K1 F2m

-------------------------------------------------------------------------------
-- Parameters
-------------------------------------------------------------------------------

-- | Coefficient @A@ of SECT239K1 curve
_a :: F2m
_a = 0
{-# INLINE _a #-}

-- | Coefficient @B@ of SECT239K1 curve
_b :: F2m
_b = 1
{-# INLINE _b #-}

-- | Polynomial of SECT239K1 curve
_f = split (witness :: F2m)
{-# INLINE _f #-}

-- | Generator of SECT239K1 curve
_g :: P
_g = A 0x29a0b6a887a983e9730988a68727a8b2d126c44cc2cc7b2a6555193035dc
       0x76310804f12e549bdb011c103089e73510acb275fc312a5dc6b76553f0ca
{-# INLINE _g #-}

-- | Cofactor of SECT239K1 curve
_h :: Integer
_h = 4
{-# INLINE _h #-}

-- | Order of SECT239K1 curve
_n :: Integer
_n = 0x2000000000000000000000000000005a79fec67cb6e91f1c1da800e478a5
{-# INLINE _n #-}
