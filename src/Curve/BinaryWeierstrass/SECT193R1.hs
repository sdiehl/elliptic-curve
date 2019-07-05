module Curve.BinaryWeierstrass.SECT193R1
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

-- | SECT193R1 curve
data SECT193R1

-- | Field of SECT193R1 curve
data FX
instance IrreducibleMonic F2 FX where
  split _ = x ^ (193 :: Int) + x ^ (15 :: Int) + 1
type F2m = Fm FX

-- | SECT193R1 curve is a binary Weierstrass curve
instance BWCurve SECT193R1 FX where
  _a _ = 0x0017858feb7a98975169e171f77b4087de098ac8a911df7b01
  {-# INLINE _a #-}
  _b _ = 0x00fdfb49bfe6c3a89facadaa7a1e5bbc7cc1c2e5d831478814
  {-# INLINE _b #-}
  _f _ = witness :: FX
  {-# INLINE _f #-}
  _g   = A 0x01f481bc5f0ff84a74ad6cdf6fdef4bf6179625372d8c0c5e1
           0x0025e399f2903712ccf3ea9e3a1ad17fb0b3201b6af7ce1b05
  {-# INLINE _g #-}
  _h _ = 2
  {-# INLINE _h #-}
  _n _ = 0x01000000000000000000000000c7f34a778f443acc920eba49
  {-# INLINE _n #-}

-- | Point of SECT193R1 curve
type P = BWPoint SECT193R1 F2m
