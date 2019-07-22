module Curve.Weierstrass.BN254B
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

-- | BN254B curve
data BN254B

-- | Field of BN254B curve
type Fp = PrimeField 0x2523648240000001ba344d80000000086121000000000013a700000000000013

-- | BN254B curve is a Weierstrass curve
instance WCurve BN254B Fp where
  a_ = const _a
  {-# INLINE a_ #-}
  b_ = const _b
  {-# INLINE b_ #-}
  g_ = _g
  {-# INLINE g_ #-}
  h_ = const _h
  {-# INLINE h_ #-}
  n_ = const _n
  {-# INLINE n_ #-}
  p_ = const _p
  {-# INLINE p_ #-}

-- | Point of BN254B curve
type P = WPoint BN254B Fp

-------------------------------------------------------------------------------
-- Parameters
-------------------------------------------------------------------------------

-- | Coefficient @A@ of BN254B curve
_a :: Fp
_a = 0
{-# INLINE _a #-}

-- | Coefficient @B@ of BN254B curve
_b :: Fp
_b = 2
{-# INLINE _b #-}

-- | Generator of BN254B curve
_g :: P
_g = A
     0x2523648240000001ba344d80000000086121000000000013a700000000000012
     1
{-# INLINE _g #-}

-- | Cofactor of BN254B curve
_h :: Integer
_h = 1
{-# INLINE _h #-}

-- | Order of BN254B curve
_n :: Integer
_n = 0x2523648240000001ba344d8000000007ff9f800000000010a10000000000000d
{-# INLINE _n #-}

-- | Characteristic of BN254B curve
_p :: Integer
_p = 0x2523648240000001ba344d80000000086121000000000013a700000000000013
{-# INLINE _p #-}
