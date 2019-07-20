module Curve.Weierstrass.BN254
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

-- | BN254 curve
data BN254

-- | Field of BN254 curve
type Fp = PrimeField 0x30644e72e131a029b85045b68181585d97816a916871ca8d3c208c16d87cfd47

-- | BN254 curve is a Weierstrass curve
instance WCurve BN254 Fp where
  a_ = const _a
  {-# INLINE a_ #-}
  b_ = const _b
  {-# INLINE b_ #-}
  g_ = _g
  {-# INLINE g_ #-}

-- | Point of BN254 curve
type P = WPoint BN254 Fp

-------------------------------------------------------------------------------
-- Parameters
-------------------------------------------------------------------------------

-- | Coefficient @A@ of BN254 curve
_a :: Fp
_a = 0
{-# INLINE _a #-}

-- | Coefficient @B@ of BN254 curve
_b :: Fp
_b = 3
{-# INLINE _b #-}

-- | Generator of BN254 curve
_g :: P
_g = A
     1
     2
{-# INLINE _g #-}

-- | Cofactor of BN254 curve
_h :: Integer
_h = 1
{-# INLINE _h #-}

-- | Order of BN254 curve
_n :: Integer
_n = 0x30644e72e131a029b85045b68181585d2833e84879b9709143e1f593f0000001
{-# INLINE _n #-}

-- | Characteristic of BN254 curve
_p :: Integer
_p = 0x30644e72e131a029b85045b68181585d97816a916871ca8d3c208c16d87cfd47
{-# INLINE _p #-}
