module Curve.BinaryWeierstrass.SECT163R1
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

-- | SECT163R1 curve
data SECT163R1

-- | Polynomial of SECT163R1 curve
data FX
instance IrreducibleMonic F2 FX where
  split _ = x ^ (163 :: Int) + x ^ (7 :: Int) + x ^ (6 :: Int) + x ^ (3 :: Int) + 1

-- | Field of SECT163R1 curve
type F2m = Fm FX

-- | SECT163R1 curve is a binary Weierstrass curve
instance BWCurve SECT163R1 FX where
  _a _ = 0x07B6882CAAEFA84F9554FF8428BD88E246D2782AE2
  {-# INLINE _a #-}
  _b _ = 0x0713612DCDDCB40AAB946BDA29CA91F73AF958AFD9
  {-# INLINE _b #-}
  _f _ = witness :: FX
  {-# INLINE _f #-}
  _g   = notImplemented
  {-# INLINE _g #-}
  _h _ = 2
  {-# INLINE _h #-}
  _r _ = 0x03FFFFFFFFFFFFFFFFFFFF48AAB689C29CA710279B
  {-# INLINE _r #-}

-- | Point of SECT163R1 curve
type P = BWPoint SECT163R1 F2m
