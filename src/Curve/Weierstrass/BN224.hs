module Curve.Weierstrass.BN224
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

-- | BN224 curve
data BN224

-- | Field of BN224 curve
type Fp = PrimeField 0xfffffffffff107288ec29e602c4520db42180823bb907d1287127833

-- | BN224 curve is a Weierstrass curve
instance WCurve BN224 Fp where
  a_ = const _a
  {-# INLINE a_ #-}
  b_ = const _b
  {-# INLINE b_ #-}
  g_ = _g
  {-# INLINE g_ #-}

-- | Point of BN224 curve
type P = WPoint BN224 Fp

-------------------------------------------------------------------------------
-- Parameters
-------------------------------------------------------------------------------

-- | Coefficient @A@ of BN224 curve
_a :: Fp
_a = 0
{-# INLINE _a #-}

-- | Coefficient @B@ of BN224 curve
_b :: Fp
_b = 3
{-# INLINE _b #-}

-- | Generator of BN224 curve
_g :: P
_g = A
     1
     2
{-# INLINE _g #-}

-- | Cofactor of BN224 curve
_h :: Integer
_h = 1
{-# INLINE _h #-}

-- | Order of BN224 curve
_n :: Integer
_n = 0xfffffffffff107288ec29e602c4420db4218082b36c2accff76c58ed
{-# INLINE _n #-}

-- | Characteristic of BN224 curve
_p :: Integer
_p = 0xfffffffffff107288ec29e602c4520db42180823bb907d1287127833
{-# INLINE _p #-}
