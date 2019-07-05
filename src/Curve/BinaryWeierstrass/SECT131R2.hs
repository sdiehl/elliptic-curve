module Curve.BinaryWeierstrass.SECT131R2
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

-- | SECT131R2 curve
data SECT131R2

-- | Polynomial of SECT131R2 curve
data FX
instance IrreducibleMonic F2 FX where
  split _ = x ^ (131 :: Int) + x ^ (8 :: Int) + x ^ (3 :: Int) + x ^ (2 :: Int) + 1

-- | Field of SECT131R2 curve
type F2m = Fm FX

-- | SECT131R2 curve is a binary Weierstrass curve
instance BWCurve SECT131R2 FX where
  _a _ = 0x03E5A88919D7CAFCBF415F07C2176573B2
  {-# INLINE _a #-}
  _b _ = 0x04B8266A46C55657AC734CE38F018F2192
  {-# INLINE _b #-}
  _f _ = witness :: FX
  {-# INLINE _f #-}
  _g   = notImplemented
  {-# INLINE _g #-}
  _h _ = 2
  {-# INLINE _h #-}
  _r _ = 0x0400000000000000016954A233049BA98F
  {-# INLINE _r #-}

-- | Point of SECT131R2 curve
type P = BWPoint SECT131R2 F2m
