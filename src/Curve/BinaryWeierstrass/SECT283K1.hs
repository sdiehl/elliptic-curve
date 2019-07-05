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

-- | Polynomial of SECT283K1 curve
data FX
instance IrreducibleMonic F2 FX where
  split _ = x ^ (283 :: Int) + x ^ (12 :: Int) + x ^ (7 :: Int) + x ^ (5 :: Int) + 1

-- | Field of SECT283K1 curve
type F2m = Fm FX

-- | SECT283K1 curve is a binary Weierstrass curve
instance BWCurve SECT283K1 FX where
  _a _ = 0
  {-# INLINE _a #-}
  _b _ = 1
  {-# INLINE _b #-}
  _f _ = witness :: FX
  {-# INLINE _f #-}
  _g   = notImplemented
  {-# INLINE _g #-}
  _h _ = 4
  {-# INLINE _h #-}
  _r _ = 0x01FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFE9AE2ED07577265DFF7F94451E061E163C61
  {-# INLINE _r #-}

-- | Point of SECT283K1 curve
type P = BWPoint SECT283K1 F2m
