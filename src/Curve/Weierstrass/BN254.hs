module Curve.Weierstrass.BN254
  ( Curve(..)
  , Fq
  , Fr
  , Group(..)
  , P
  , Point(..)
  , WPoint
  , WCurve(..)
  , _a
  , _b
  , _g
  , _h
  , _q
  , _r
  , _x
  , _y
  ) where

import Protolude

import PrimeField (PrimeField)

import Curve (Curve(..))
import Curve.Weierstrass (Point(..), WCurve(..), WPoint)
import Group (Group(..))

-------------------------------------------------------------------------------
-- Types
-------------------------------------------------------------------------------

-- | BN254 curve.
data BN254

-- | Field of points of BN254 curve.
type Fq = PrimeField 0x30644e72e131a029b85045b68181585d97816a916871ca8d3c208c16d87cfd47

-- | Field of coefficients of BN254 curve.
type Fr = PrimeField 0x30644e72e131a029b85045b68181585d2833e84879b9709143e1f593f0000001

-- | BN254 curve is a Weierstrass curve.
instance WCurve BN254 Fq where
  a_ = const _a
  {-# INLINE a_ #-}
  b_ = const _b
  {-# INLINE b_ #-}
  g_ = _g
  {-# INLINE g_ #-}
  h_ = const _h
  {-# INLINE h_ #-}
  q_ = const _q
  {-# INLINE q_ #-}
  r_ = const _r
  {-# INLINE r_ #-}
  x_ = const _x
  {-# INLINE x_ #-}
  y_ = const _y
  {-# INLINE y_ #-}

-- | Point of BN254 curve.
type P = WPoint BN254 Fq

-------------------------------------------------------------------------------
-- Parameters
-------------------------------------------------------------------------------

-- | Coefficient @A@ of BN254 curve.
_a :: Fq
_a = 0x0
{-# INLINE _a #-}

-- | Coefficient @B@ of BN254 curve.
_b :: Fq
_b = 0x3
{-# INLINE _b #-}

-- | Generator of BN254 curve.
_g :: P
_g = A _x _y
{-# INLINE _g #-}

-- | Cofactor of BN254 curve.
_h :: Integer
_h = 0x1
{-# INLINE _h #-}

-- | Characteristic of BN254 curve.
_q :: Integer
_q = 0x30644e72e131a029b85045b68181585d97816a916871ca8d3c208c16d87cfd47
{-# INLINE _q #-}

-- | Order of BN254 curve.
_r :: Integer
_r = 0x30644e72e131a029b85045b68181585d2833e84879b9709143e1f593f0000001
{-# INLINE _r #-}

-- | Coordinate @X@ of BN254 curve.
_x :: Fq
_x = 0x1
{-# INLINE _x #-}

-- | Coordinate @Y@ of BN254 curve.
_y :: Fq
_y = 0x2
{-# INLINE _y #-}
