module Curve.BinaryWeierstrass.SECT283K1
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

-- | SECT283K1 curve
data SECT283K1

-- | Field of SECT283K1 curve
data FX
instance IrreducibleMonic F2 FX where
  split _ = x ^ (283 :: Int) + x ^ (12 :: Int) + x ^ (7 :: Int) + x ^ (5 :: Int) + 1
type F2m = Fm FX

-- | SECT283K1 curve is a binary Weierstrass curve
instance BWCurve SECT283K1 FX where
  a_ = const _a 
  {-# INLINE a_ #-}
  b_ = const _b
  {-# INLINE b_ #-}
  g_ = _g
  {-# INLINE g_ #-}

-- | Point of SECT283K1 curve
type P = BWPoint SECT283K1 F2m

-------------------------------------------------------------------------------
-- Parameters
-------------------------------------------------------------------------------

-- | Coefficient @A@ of SECT283K1 curve
_a :: F2m
_a = 0
{-# INLINE _a #-}

-- | Coefficient @B@ of SECT283K1 curve
_b :: F2m
_b = 1
{-# INLINE _b #-}

-- | Polynomial of SECT283K1 curve
_f = split (witness :: F2m)
{-# INLINE _f #-}

-- | Generator of SECT283K1 curve
_g :: P
_g = A 0x0503213f78ca44883f1a3b8162f188e553cd265f23c1567a16876913b0c2ac2458492836
       0x01ccda380f1c9e318d90f95d07e5426fe87e45c0e8184698e45962364e34116177dd2259
{-# INLINE _g #-}

-- | Cofactor of SECT283K1 curve
_h :: Integer
_h = 4
{-# INLINE _h #-}

-- | Order of SECT283K1 curve
_n :: Integer
_n = 0x01ffffffffffffffffffffffffffffffffffe9ae2ed07577265dff7f94451e061e163c61
{-# INLINE _n #-}
