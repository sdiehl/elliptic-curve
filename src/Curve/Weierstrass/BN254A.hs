module Curve.Weierstrass.BN254A
  ( AP
  , Curve(..)
  , Fq
  , Fr
  , Group(..)
  , Point(..)
  , WCurve(..)
  , WPoint
  , WACurve(..)
  , WAPoint
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

import Curve (Curve(..), Form(..))
import Curve.Weierstrass (Point(..), WCurve(..), WPoint, WACurve(..), WAPoint)
import Group (Group(..))

-------------------------------------------------------------------------------
-- BN254A curve
-------------------------------------------------------------------------------

-- | BN254A curve.
data BN254A

-- | Field of points of BN254A curve.
type Fq = PrimeField 0x2370fb049d410fbe4e761a9886e502417d023f40180000017e80600000000001

-- | Field of coefficients of BN254A curve.
type Fr = PrimeField 0x2370fb049d410fbe4e761a9886e502411dc1af70120000017e80600000000001

-- | BN254A curve is a Weierstrass curve.
instance Curve 'Weierstrass c BN254A Fq => WCurve c BN254A Fq where
  a_ = const _a
  {-# INLINE a_ #-}
  b_ = const _b
  {-# INLINE b_ #-}
  h_ = const _h
  {-# INLINE h_ #-}
  q_ = const _q
  {-# INLINE q_ #-}
  r_ = const _r
  {-# INLINE r_ #-}

-- | Coefficient @A@ of BN254A curve.
_a :: Fq
_a = 0x0
{-# INLINE _a #-}

-- | Coefficient @B@ of BN254A curve.
_b :: Fq
_b = 0x5
{-# INLINE _b #-}

-- | Cofactor of BN254A curve.
_h :: Integer
_h = 0x1
{-# INLINE _h #-}

-- | Characteristic of BN254A curve.
_q :: Integer
_q = 0x2370fb049d410fbe4e761a9886e502417d023f40180000017e80600000000001
{-# INLINE _q #-}

-- | Order of BN254A curve.
_r :: Integer
_r = 0x2370fb049d410fbe4e761a9886e502411dc1af70120000017e80600000000001
{-# INLINE _r #-}

-------------------------------------------------------------------------------
-- Affine coordinates
-------------------------------------------------------------------------------

-- | Affine BN254A point.
type AP = WAPoint BN254A Fq

-- | Affine BN254A curve is a Weierstrass affine curve.
instance WACurve BN254A Fq where
  g_ = _g
  {-# INLINE g_ #-}
  x_ = const _x
  {-# INLINE x_ #-}
  y_ = const _y
  {-# INLINE y_ #-}

-- | Generator of affine BN254A curve.
_g :: AP
_g = A _x _y
{-# INLINE _g #-}

-- | Coordinate @X@ of affine BN254A curve.
_x :: Fq
_x = 0x1
{-# INLINE _x #-}

-- | Coordinate @Y@ of affine BN254A curve.
_y :: Fq
_y = 0xd45589b158faaf6ab0e4ad38d998e9982e7ff63964ee1460342a592677cccb0
{-# INLINE _y #-}
