module Curve.Weierstrass.BrainpoolP160R1
  ( Fp
  , P
  , _a
  , _b
  , _g
  , _h
  , _n
  , _p
  ) where

import Protolude

import PrimeField (PrimeField)

import Curve.Weierstrass (Point(..), WCurve(..), WPoint)

-------------------------------------------------------------------------------
-- Types
-------------------------------------------------------------------------------

-- | Brainpool-P160R1 curve
data BrainpoolP160R1

-- | Field of Brainpool-P160R1 curve
type Fp = PrimeField 0xe95e4a5f737059dc60dfc7ad95b3d8139515620f

-- | Brainpool-P160R1 curve is a Weierstrass curve
instance WCurve BrainpoolP160R1 Fp where
  a_ = const _a
  {-# INLINE a_ #-}
  b_ = const _b
  {-# INLINE b_ #-}
  g_ = _g
  {-# INLINE g_ #-}

-- | Point of Brainpool-P160R1 curve
type P = WPoint BrainpoolP160R1 Fp

-------------------------------------------------------------------------------
-- Parameters
-------------------------------------------------------------------------------

-- | Coefficient @A@ of Brainpool-P160R1 curve
_a :: Fp
_a = 0x340e7be2a280eb74e2be61bada745d97e8f7c300
{-# INLINE _a #-}

-- | Coefficient @B@ of Brainpool-P160R1 curve
_b :: Fp
_b = 0x1e589a8595423412134faa2dbdec95c8d8675e58
{-# INLINE _b #-}

-- | Generator of Brainpool-P160R1 curve
_g :: P
_g = A
     0xbed5af16ea3f6a4f62938c4631eb5af7bdbcdbc3
     0x1667cb477a1a8ec338f94741669c976316da6321
{-# INLINE _g #-}

-- | Cofactor of Brainpool-P160R1 curve
_h :: Integer
_h = 1
{-# INLINE _h #-}

-- | Order of Brainpool-P160R1 curve
_n :: Integer
_n = 0xe95e4a5f737059dc60df5991d45029409e60fc09
{-# INLINE _n #-}

-- | Characteristic of Brainpool-P160R1 curve
_p :: Integer
_p = 0xe95e4a5f737059dc60dfc7ad95b3d8139515620f
{-# INLINE _p #-}
