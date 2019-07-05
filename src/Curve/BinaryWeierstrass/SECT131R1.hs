module Curve.BinaryWeierstrass.SECT131R1
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

-- | SECT131R1 curve
data SECT131R1

-- | Polynomial of SECT131R1 curve
data FX
instance IrreducibleMonic F2 FX where
  split _ = x ^ (131 :: Int) + x ^ (8 :: Int) + x ^ (3 :: Int) + x ^ (2 :: Int) + 1

-- | Field of SECT131R1 curve
type F2m = Fm FX

-- | SECT131R1 curve is a binary Weierstrass curve
instance BWCurve SECT131R1 FX where
  _a _ = 0x07A11B09A76B562144418FF3FF8C2570B8
  {-# INLINE _a #-}
  _b _ = 0x0217C05610884B63B9C6C7291678F9D341
  {-# INLINE _b #-}
  _f _ = witness :: FX
  {-# INLINE _f #-}
  _g   = notImplemented
  {-# INLINE _g #-}
  _h _ = 2
  {-# INLINE _h #-}
  _r _ = 0x0400000000000000023123953A9464B54D
  {-# INLINE _r #-}

-- | Point of SECT131R1 curve
type P = BWPoint SECT131R1 F2m
