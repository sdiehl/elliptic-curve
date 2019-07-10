module Curve.ShortWeierstrass.BrainpoolP192T1
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

import Curve.ShortWeierstrass (Point(..), SWCurve(..), SWPoint)

-------------------------------------------------------------------------------
-- Types
-------------------------------------------------------------------------------

-- | Brainpool-P192T1 curve
data BrainpoolP192T1

-- | Field of Brainpool-P192T1 curve
type Fp = PrimeField 0xc302f41d932a36cda7a3463093d18db78fce476de1a86297

-- | Brainpool-P192T1 curve is a short Weierstrass curve
instance SWCurve BrainpoolP192T1 Fp where
  a_ = const _a
  {-# INLINE a_ #-}
  b_ = const _b
  {-# INLINE b_ #-}
  g_ = _g
  {-# INLINE g_ #-}

-- | Point of Brainpool-P192T1 curve
type P = SWPoint BrainpoolP192T1 Fp

-------------------------------------------------------------------------------
-- Parameters
-------------------------------------------------------------------------------

-- | Coefficient @A@ of Brainpool-P192T1 curve
_a :: Fp
_a = 0xc302f41d932a36cda7a3463093d18db78fce476de1a86294
{-# INLINE _a #-}

-- | Coefficient @B@ of Brainpool-P192T1 curve
_b :: Fp
_b = 0x13d56ffaec78681e68f9deb43b35bec2fb68542e27897b79
{-# INLINE _b #-}

-- | Generator of Brainpool-P192T1 curve
_g :: P
_g = A
     0x3ae9e58c82f63c30282e1fe7bbf43fa72c446af6f4618129
     0x097e2c5667c2223a902ab5ca449d0084b7e5b3de7ccc01c9
{-# INLINE _g #-}

-- | Cofactor of Brainpool-P192T1 curve
_h :: Integer
_h = 1
{-# INLINE _h #-}

-- | Order of Brainpool-P192T1 curve
_n :: Integer
_n = 0xc302f41d932a36cda7a3462f9e9e916b5be8f1029ac4acc1
{-# INLINE _n #-}

-- | Characteristic of Brainpool-P192T1 curve
_p :: Integer
_p = 0xc302f41d932a36cda7a3463093d18db78fce476de1a86297
{-# INLINE _p #-}
