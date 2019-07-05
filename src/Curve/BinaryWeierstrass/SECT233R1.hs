module Curve.BinaryWeierstrass.SECT233R1
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

-- | SECT233R1 curve
data SECT233R1

-- | Field of SECT233R1 curve
data FX
instance IrreducibleMonic F2 FX where
  split _ = x ^ (233 :: Int) + x ^ (74 :: Int) + 1
type F2m = Fm FX

-- | SECT233R1 curve is a binary Weierstrass curve
instance BWCurve SECT233R1 FX where
  _a _ = 1
  {-# INLINE _a #-}
  _b _ = 0x0066647ede6c332c7f8c0923bb58213b333b20e9ce4281fe115f7d8f90ad
  {-# INLINE _b #-}
  _f _ = witness :: FX
  {-# INLINE _f #-}
  _g   = A 0x00fac9dfcbac8313bb2139f1bb755fef65bc391f8b36f8f8eb7371fd558b
           0x01006a08a41903350678e58528bebf8a0beff867a7ca36716f7e01f81052
  {-# INLINE _g #-}
  _h _ = 2
  {-# INLINE _h #-}
  _n _ = 0x01000000000000000000000000000013e974e72f8a6922031d2603cfe0d7
  {-# INLINE _n #-}

-- | Point of SECT233R1 curve
type P = BWPoint SECT233R1 F2m
