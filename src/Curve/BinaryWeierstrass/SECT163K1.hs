module Curve.BinaryWeierstrass.SECT163K1
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

-- | SECT163K1 curve
data SECT163K1

-- | Polynomial of SECT163K1 curve
data FX
instance IrreducibleMonic F2 FX where
  split _ = x ^ (163 :: Int) + x ^ (7 :: Int) + x ^ (6 :: Int) + x ^ (3 :: Int) + 1

-- | Field of SECT163K1 curve
type F2m = Fm FX

-- | SECT163K1 curve is a binary Weierstrass curve
instance BWCurve SECT163K1 FX where
  _a _ = 1
  {-# INLINE _a #-}
  _b _ = 1
  {-# INLINE _b #-}
  _f _ = witness :: FX
  {-# INLINE _f #-}
  _g   = notImplemented
  {-# INLINE _g #-}
  _h _ = 2
  {-# INLINE _h #-}
  _r _ = 0x04000000000000000000020108A2E0CC0D99F8A5EF
  {-# INLINE _r #-}

-- | Point of SECT163K1 curve
type P = BWPoint SECT163K1 F2m
