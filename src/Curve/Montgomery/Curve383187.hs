module Curve.Montgomery.Curve383187
  ( AP
  , Curve(..)
  , Fq
  , Fr
  , Group(..)
  , MCurve(..)
  , MPoint
  , MACurve(..)
  , MAPoint
  , Point(..)
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
import Curve.Montgomery (MCurve(..), MPoint, MACurve(..), MAPoint, Point(..))
import Group (Group(..))

-------------------------------------------------------------------------------
-- Curve383187 curve
-------------------------------------------------------------------------------

-- | Curve383187 curve.
data Curve383187

-- | Field of points of Curve383187 curve.
type Fq = PrimeField 0x7fffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff45

-- | Field of coefficients of Curve383187 curve.
type Fr = PrimeField 0x1000000000000000000000000000000000000000000000000e85a85287a1488acd41ae84b2b7030446f72088b00a0e21

-- | Curve383187 curve is a Montgomery curve.
instance Curve 'Montgomery c Curve383187 Fq => MCurve c Curve383187 Fq where
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

-- | Coefficient @A@ of Curve383187 curve.
_a :: Fq
_a = 0x38251
{-# INLINE _a #-}

-- | Coefficient @B@ of Curve383187 curve.
_b :: Fq
_b = 0x1
{-# INLINE _b #-}

-- | Cofactor of Curve383187 curve.
_h :: Integer
_h = 0x8
{-# INLINE _h #-}

-- | Characteristic of Curve383187 curve.
_q :: Integer
_q = 0x7fffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff45
{-# INLINE _q #-}

-- | Order of Curve383187 curve.
_r :: Integer
_r = 0x1000000000000000000000000000000000000000000000000e85a85287a1488acd41ae84b2b7030446f72088b00a0e21
{-# INLINE _r #-}

-------------------------------------------------------------------------------
-- Affine coordinates
-------------------------------------------------------------------------------

-- | Affine Curve383187 point.
type AP = MAPoint Curve383187 Fq

-- | Affine Curve383187 curve is a Montgomery affine curve.
instance MACurve Curve383187 Fq where
  gA_ = gA
  {-# INLINE gA_ #-}
  xA_ = const xA
  {-# INLINE xA_ #-}
  yA_ = const yA
  {-# INLINE yA_ #-}

-- | Generator of affine Curve383187 curve.
gA :: AP
gA = A xA yA
{-# INLINE gA #-}

-- | Coordinate @X@ of affine Curve383187 curve.
xA :: Fq
xA = 0x5
{-# INLINE xA #-}

-- | Coordinate @Y@ of affine Curve383187 curve.
yA :: Fq
yA = 0x1eebe07dc1871896732b12d5504a32370471965c7a11f2c89865f855ab3cbd7c224e3620c31af3370788457dd5ce46df
{-# INLINE yA #-}
