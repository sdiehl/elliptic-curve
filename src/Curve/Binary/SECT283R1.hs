module Curve.Binary.SECT283R1
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

import BinaryField (BinaryField)

import Curve.Binary (BCurve(..), BPoint, Point(..))

-------------------------------------------------------------------------------
-- Types
-------------------------------------------------------------------------------

-- | SECT283R1 curve
data SECT283R1

-- | Field of SECT283R1 curve
type F2m = BinaryField 0x800000000000000000000000000000000000000000000000000000000000000000010a1

-- | SECT283R1 curve is a binary curve
instance BCurve SECT283R1 F2m where
  a_ = const _a
  {-# INLINE a_ #-}
  b_ = const _b
  {-# INLINE b_ #-}
  g_ = _g
  {-# INLINE g_ #-}

-- | Point of SECT283R1 curve
type P = BPoint SECT283R1 F2m

-------------------------------------------------------------------------------
-- Parameters
-------------------------------------------------------------------------------

-- | Coefficient @A@ of SECT283R1 curve
_a :: F2m
_a = 1
{-# INLINE _a #-}

-- | Coefficient @B@ of SECT283R1 curve
_b :: F2m
_b = 0x27b680ac8b8596da5a4af8a19a0303fca97fd7645309fa2a581485af6263e313b79a2f5
{-# INLINE _b #-}

-- | Polynomial of SECT283R1 curve
_f :: Integer
_f = 0x800000000000000000000000000000000000000000000000000000000000000000010a1
{-# INLINE _f #-}

-- | Generator of SECT283R1 curve
_g :: P
_g = A
     0x5f939258db7dd90e1934f8c70b0dfec2eed25b8557eac9c80e2e198f8cdbecd86b12053
     0x3676854fe24141cb98fe6d4b20d02b4516ff702350eddb0826779c813f0df45be8112f4
{-# INLINE _g #-}

-- | Cofactor of SECT283R1 curve
_h :: Integer
_h = 2
{-# INLINE _h #-}

-- | Order of SECT283R1 curve
_n :: Integer
_n = 0x3ffffffffffffffffffffffffffffffffffef90399660fc938a90165b042a7cefadb307
{-# INLINE _n #-}
