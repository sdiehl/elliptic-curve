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

-- | Field of SECT131R2 curve
data FX
instance IrreducibleMonic F2 FX where
  split _ = x ^ (131 :: Int) + x ^ (8 :: Int) + x ^ (3 :: Int) + x ^ (2 :: Int) + 1
type F2m = Fm FX

-- | SECT131R2 curve is a binary Weierstrass curve
instance BWCurve SECT131R2 FX where
  _a _ = 0x03e5a88919d7cafcbf415f07c2176573b2
  {-# INLINE _a #-}
  _b _ = 0x04b8266a46c55657ac734ce38f018f2192
  {-# INLINE _b #-}
  _f _ = witness :: FX
  {-# INLINE _f #-}
  _g   = A 0x0356dcd8f2f95031ad652d23951bb366a8
           0x0648f06d867940a5366d9e265de9eb240f
  {-# INLINE _g #-}
  _h _ = 2
  {-# INLINE _h #-}
  _n _ = 0x0400000000000000016954a233049ba98f
  {-# INLINE _n #-}

-- | Point of SECT131R2 curve
type P = BWPoint SECT131R2 F2m
