module Curve.BinaryWeierstrass.SECT233K1
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

-- | SECT233K1 curve
data SECT233K1

-- | Polynomial of SECT233K1 curve
data FX
instance IrreducibleMonic F2 FX where
  split _ = x ^ (233 :: Int) + x ^ (74 :: Int) + 1

-- | Field of SECT233K1 curve
type F2m = Fm FX

-- | SECT233K1 curve is a binary Weierstrass curve
instance BWCurve SECT233K1 FX where
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
  _r _ = 0x8000000000000000000000000000069D5BB915BCD46EFB1AD5F173ABDF
  {-# INLINE _r #-}

-- | Point of SECT233K1 curve
type P = BWPoint SECT233K1 F2m
