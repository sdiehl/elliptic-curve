module Curve.BinaryWeierstrass.SECT283R1
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

-- | SECT283R1 curve
data SECT283R1

-- | Field of SECT283R1 curve
data FX
instance IrreducibleMonic F2 FX where
  split _ = x ^ (283 :: Int) + x ^ (12 :: Int) + x ^ (7 :: Int) + x ^ (5 :: Int) + 1
type F2m = Fm FX

-- | SECT283R1 curve is a binary Weierstrass curve
instance BWCurve SECT283R1 FX where
  _a _ = 1
  {-# INLINE _a #-}
  _b _ = 0x027b680ac8b8596da5a4af8a19a0303fca97fd7645309fa2a581485af6263e313b79a2f5
  {-# INLINE _b #-}
  _f _ = witness :: FX
  {-# INLINE _f #-}
  _g   = A 0x05f939258db7dd90e1934f8c70b0dfec2eed25b8557eac9c80e2e198f8cdbecd86b12053
           0x03676854fe24141cb98fe6d4b20d02b4516ff702350eddb0826779c813f0df45be8112f4
  {-# INLINE _g #-}
  _h _ = 2
  {-# INLINE _h #-}
  _n _ = 0x03ffffffffffffffffffffffffffffffffffef90399660fc938a90165b042a7cefadb307
  {-# INLINE _n #-}

-- | Point of SECT283R1 curve
type P = BWPoint SECT283R1 F2m
