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

-- | Field of SECT163K1 curve
data FX
instance IrreducibleMonic F2 FX where
  split _ = x ^ (163 :: Int) + x ^ (7 :: Int) + x ^ (6 :: Int) + x ^ (3 :: Int) + 1
type F2m = Fm FX

-- | SECT163K1 curve is a binary Weierstrass curve
instance BWCurve SECT163K1 FX where
  _a _ = 1
  {-# INLINE _a #-}
  _b _ = 1
  {-# INLINE _b #-}
  _f _ = witness :: FX
  {-# INLINE _f #-}
  _g   = A 0x02fe13c0537bbc11acaa07d793de4e6d5e5c94eee8
           0x0289070fb05d38ff58321f2e800536d538ccdaa3d9
  {-# INLINE _g #-}
  _h _ = 2
  {-# INLINE _h #-}
  _n _ = 0x04000000000000000000020108a2e0cc0d99f8a5ef
  {-# INLINE _n #-}

-- | Point of SECT163K1 curve
type P = BWPoint SECT163K1 F2m
