module Curve.ShortWeierstrass.BN128.G1
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

-- | BN128 curve G1
data G1

-- | Field of BN128 curve G1
type Fp = PrimeField 0x30644e72e131a029b85045b68181585d97816a916871ca8d3c208c16d87cfd47

-- | BN128 curve G1 is a short Weierstrass curve
instance SWCurve G1 Fp where
  a_ = const _a
  {-# INLINE a_ #-}
  b_ = const _b
  {-# INLINE b_ #-}
  g_ = _g
  {-# INLINE g_ #-}

-- | Point of BN128 curve G1
type P = SWPoint G1 Fp

-------------------------------------------------------------------------------
-- Parameters
-------------------------------------------------------------------------------

-- | Coefficient @A@ of BN128 curve G1
_a :: Fp
_a = 0
{-# INLINE _a #-}

-- | Coefficient @B@ of BN128 curve G1
_b :: Fp
_b = 3
{-# INLINE _b #-}

-- | Generator of BN128 curve G1
_g :: P
_g = A 1 2
{-# INLINE _g #-}

-- | Cofactor of BN128 curve G1
_h :: Integer
_h = 1
{-# INLINE _h #-}

-- | Order of BN128 curve G1
_n :: Integer
_n = 0x30644e72e131a029b85045b68181585d2833e84879b9709143e1f593f0000001
{-# INLINE _n #-}

-- | Characteristic of BN128 curve G1
_p :: Integer
_p = 0x30644e72e131a029b85045b68181585d97816a916871ca8d3c208c16d87cfd47
{-# INLINE _p #-}
