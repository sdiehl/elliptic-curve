module Curve.BinaryWeierstrass.SECT239K1
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

-- | SECT239K1 curve
data SECT239K1

-- | Field of SECT239K1 curve
data FX
instance IrreducibleMonic F2 FX where
  split _ = x ^ (239 :: Int) + x ^ (158 :: Int) + 1
type F2m = Fm FX

-- | SECT239K1 curve is a binary Weierstrass curve
instance BWCurve SECT239K1 FX where
  _a _ = 0
  {-# INLINE _a #-}
  _b _ = 1
  {-# INLINE _b #-}
  _f _ = witness :: FX
  {-# INLINE _f #-}
  _g   = A 0x29a0b6a887a983e9730988a68727a8b2d126c44cc2cc7b2a6555193035dc
           0x76310804f12e549bdb011c103089e73510acb275fc312a5dc6b76553f0ca
  {-# INLINE _g #-}
  _h _ = 4
  {-# INLINE _h #-}
  _n _ = 0x2000000000000000000000000000005a79fec67cb6e91f1c1da800e478a5
  {-# INLINE _n #-}

-- | Point of SECT239K1 curve
type P = BWPoint SECT239K1 F2m
