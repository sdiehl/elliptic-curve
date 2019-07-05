module Curve.BinaryWeierstrass.SECT283K1
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

-- | SECT283K1 curve
data SECT283K1

-- | Field of SECT283K1 curve
data FX
instance IrreducibleMonic F2 FX where
  split _ = x ^ (283 :: Int) + x ^ (12 :: Int) + x ^ (7 :: Int) + x ^ (5 :: Int) + 1
type F2m = Fm FX

-- | SECT283K1 curve is a binary Weierstrass curve
instance BWCurve SECT283K1 FX where
  _a _ = 0
  {-# INLINE _a #-}
  _b _ = 1
  {-# INLINE _b #-}
  _f _ = witness :: FX
  {-# INLINE _f #-}
  _g   = A 0x0503213f78ca44883f1a3b8162f188e553cd265f23c1567a16876913b0c2ac2458492836
           0x01ccda380f1c9e318d90f95d07e5426fe87e45c0e8184698e45962364e34116177dd2259
  {-# INLINE _g #-}
  _h _ = 4
  {-# INLINE _h #-}
  _n _ = 0x01ffffffffffffffffffffffffffffffffffe9ae2ed07577265dff7f94451e061e163c61
  {-# INLINE _n #-}

-- | Point of SECT283K1 curve
type P = BWPoint SECT283K1 F2m
