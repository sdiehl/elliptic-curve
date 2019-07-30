module Curve.Weierstrass.BN256
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
  , _h
  , _q
  , _r
  , gA
  , xA
  , yA
  ) where

import Protolude

import PrimeField (PrimeField)

import Curve (Curve(..), Form(..))
import Curve.Weierstrass (Point(..), WCurve(..), WPoint, WACurve(..), WAPoint)
import Group (Group(..))

-------------------------------------------------------------------------------
-- BN256 curve
-------------------------------------------------------------------------------

-- | BN256 curve.
data BN256

-- | Field of points of BN256 curve.
type Fq = PrimeField 0xfffffffffffcf0cd46e5f25eee71a49f0cdc65fb12980a82d3292ddbaed33013

-- | Field of coefficients of BN256 curve.
type Fr = PrimeField 0xfffffffffffcf0cd46e5f25eee71a49e0cdc65fb1299921af62d536cd10b500d

-- | BN256 curve is a Weierstrass curve.
instance Curve 'Weierstrass c BN256 Fq => WCurve c BN256 Fq where
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

-- | Coefficient @A@ of BN256 curve.
_a :: Fq
_a = 0x0
{-# INLINE _a #-}

-- | Coefficient @B@ of BN256 curve.
_b :: Fq
_b = 0x3
{-# INLINE _b #-}

-- | Cofactor of BN256 curve.
_h :: Integer
_h = 0x1
{-# INLINE _h #-}

-- | Characteristic of BN256 curve.
_q :: Integer
_q = 0xfffffffffffcf0cd46e5f25eee71a49f0cdc65fb12980a82d3292ddbaed33013
{-# INLINE _q #-}

-- | Order of BN256 curve.
_r :: Integer
_r = 0xfffffffffffcf0cd46e5f25eee71a49e0cdc65fb1299921af62d536cd10b500d
{-# INLINE _r #-}

-------------------------------------------------------------------------------
-- Affine coordinates
-------------------------------------------------------------------------------

-- | Affine BN256 point.
type AP = WAPoint BN256 Fq

-- | Affine BN256 curve is a Weierstrass affine curve.
instance WACurve BN256 Fq where
  gA_ = gA
  {-# INLINE gA_ #-}
  xA_ = const xA
  {-# INLINE xA_ #-}
  yA_ = const yA
  {-# INLINE yA_ #-}

-- | Generator of affine BN256 curve.
gA :: AP
gA = A xA yA
{-# INLINE gA #-}

-- | Coordinate @X@ of affine BN256 curve.
xA :: Fq
xA = 0x1
{-# INLINE xA #-}

-- | Coordinate @Y@ of affine BN256 curve.
yA :: Fq
yA = 0x2
{-# INLINE yA #-}
