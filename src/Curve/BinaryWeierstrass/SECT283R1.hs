module Curve.BinaryWeierstrass.SECT283R1
  -- | Types
  ( F2m
  , P
  -- | Parameters
  , _a
  , _b
  , _f
  , _g
  , _h
  , _n
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
  a_ = const _a 
  {-# INLINE a_ #-}
  b_ = const _b
  {-# INLINE b_ #-}
  g_ = _g
  {-# INLINE g_ #-}

-- | Point of SECT283R1 curve
type P = BWPoint SECT283R1 F2m

-------------------------------------------------------------------------------
-- Parameters
-------------------------------------------------------------------------------

-- | Coefficient @A@ of SECT283R1 curve
_a :: F2m
_a = 1
{-# INLINE _a #-}

-- | Coefficient @B@ of SECT283R1 curve
_b :: F2m
_b = 0x027b680ac8b8596da5a4af8a19a0303fca97fd7645309fa2a581485af6263e313b79a2f5
{-# INLINE _b #-}

-- | Polynomial of SECT283R1 curve
_f = split (witness :: F2m)
{-# INLINE _f #-}

-- | Generator of SECT283R1 curve
_g :: P
_g = A 0x05f939258db7dd90e1934f8c70b0dfec2eed25b8557eac9c80e2e198f8cdbecd86b12053
       0x03676854fe24141cb98fe6d4b20d02b4516ff702350eddb0826779c813f0df45be8112f4
{-# INLINE _g #-}

-- | Cofactor of SECT283R1 curve
_h :: Integer
_h = 2
{-# INLINE _h #-}

-- | Order of SECT283R1 curve
_n :: Integer
_n = 0x03ffffffffffffffffffffffffffffffffffef90399660fc938a90165b042a7cefadb307
{-# INLINE _n #-}
