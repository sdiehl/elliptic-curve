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

-- | Field of SECT233K1 curve
data FX
instance IrreducibleMonic F2 FX where
  split _ = x ^ (233 :: Int) + x ^ (74 :: Int) + 1
type F2m = Fm FX

-- | SECT233K1 curve is a binary Weierstrass curve
instance BWCurve SECT233K1 FX where
  _a _ = 0
  {-# INLINE _a #-}
  _b _ = 1
  {-# INLINE _b #-}
  _f _ = witness :: FX
  {-# INLINE _f #-}
  _g   = A 0x017232ba853a7e731af129f22ff4149563a419c26bf50a4c9d6eefad6126
           0x01db537dece819b7f70f555a67c427a8cd9bf18aeb9b56e0c11056fae6a3
  {-# INLINE _g #-}
  _h _ = 4
  {-# INLINE _h #-}
  _n _ = 0x8000000000000000000000000000069d5bb915bcd46efb1ad5f173abdf
  {-# INLINE _n #-}

-- | Point of SECT233K1 curve
type P = BWPoint SECT233K1 F2m
