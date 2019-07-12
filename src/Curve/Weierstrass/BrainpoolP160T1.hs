module Curve.Weierstrass.BrainpoolP160T1
  -- | Types
  ( Fp
  , P
  -- | Parameters
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

-- | Brainpool-P160T1 curve
data BrainpoolP160T1

-- | Field of Brainpool-P160T1 curve
type Fp = PrimeField 0xe95e4a5f737059dc60dfc7ad95b3d8139515620f

-- | Brainpool-P160T1 curve is a Weierstrass curve
instance WCurve BrainpoolP160T1 Fp where
  a_ = const _a
  {-# INLINE a_ #-}
  b_ = const _b
  {-# INLINE b_ #-}
  g_ = _g
  {-# INLINE g_ #-}

-- | Point of Brainpool-P160T1 curve
type P = WPoint BrainpoolP160T1 Fp

-------------------------------------------------------------------------------
-- Parameters
-------------------------------------------------------------------------------

-- | Coefficient @A@ of Brainpool-P160T1 curve
_a :: Fp
_a = 0xe95e4a5f737059dc60dfc7ad95b3d8139515620c
{-# INLINE _a #-}

-- | Coefficient @B@ of Brainpool-P160T1 curve
_b :: Fp
_b = 0x7a556b6dae535b7b51ed2c4d7daa7a0b5c55f380
{-# INLINE _b #-}

-- | Generator of Brainpool-P160T1 curve
_g :: P
_g = A
     0xb199b13b9b34efc1397e64baeb05acc265ff2378
     0xadd6718b7c7c1961f0991b842443772152c9e0ad
{-# INLINE _g #-}

-- | Cofactor of Brainpool-P160T1 curve
_h :: Integer
_h = 1
{-# INLINE _h #-}

-- | Order of Brainpool-P160T1 curve
_n :: Integer
_n = 0xe95e4a5f737059dc60df5991d45029409e60fc09
{-# INLINE _n #-}

-- | Characteristic of Brainpool-P160T1 curve
_p :: Integer
_p = 0xe95e4a5f737059dc60dfc7ad95b3d8139515620f
{-# INLINE _p #-}
