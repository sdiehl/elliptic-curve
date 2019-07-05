module Curve.BinaryWeierstrass.SECT163R2
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

-- | SECT163R2 curve
data SECT163R2

-- | Polynomial of SECT163R2 curve
data FX
instance IrreducibleMonic F2 FX where
  split _ = x ^ (163 :: Int) + x ^ (7 :: Int) + x ^ (6 :: Int) + x ^ (3 :: Int) + 1

-- | Field of SECT163R2 curve
type F2m = Fm FX

-- | SECT163R2 curve is a binary Weierstrass curve
instance BWCurve SECT163R2 FX where
  _a _ = 1
  {-# INLINE _a #-}
  _b _ = 0x020A601907B8C953CA1481EB10512F78744A3205FD
  {-# INLINE _b #-}
  _f _ = witness :: FX
  {-# INLINE _f #-}
  _g   = notImplemented
  {-# INLINE _g #-}
  _h _ = 2
  {-# INLINE _h #-}
  _r _ = 0x040000000000000000000292FE77E70C12A4234C33
  {-# INLINE _r #-}

-- | Point of SECT163R2 curve
type P = BWPoint SECT163R2 F2m
