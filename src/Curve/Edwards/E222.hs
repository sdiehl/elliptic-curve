module Curve.Edwards.E222
  ( AP
  , Curve(..)
  , ECurve(..)
  , EPoint
  , EACurve(..)
  , EAPoint
  , Fq
  , Fr
  , Group(..)
  , Point(..)
  , _a
  , _d
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
import Curve.Edwards (ECurve(..), EPoint, EACurve(..), EAPoint, Point(..))
import Group (Group(..))

-------------------------------------------------------------------------------
-- E222 curve
-------------------------------------------------------------------------------

-- | E222 curve.
data E222

-- | Field of points of E222 curve.
type Fq = PrimeField 0x3fffffffffffffffffffffffffffffffffffffffffffffffffffff8b

-- | Field of coefficients of E222 curve.
type Fr = PrimeField 0xffffffffffffffffffffffffffff70cbc95e932f802f31423598cbf

-- | E222 curve is an Edwards curve.
instance Curve 'Edwards c E222 Fq => ECurve c E222 Fq where
  a_ = const _a
  {-# INLINE a_ #-}
  d_ = const _d
  {-# INLINE d_ #-}
  h_ = const _h
  {-# INLINE h_ #-}
  q_ = const _q
  {-# INLINE q_ #-}
  r_ = const _r
  {-# INLINE r_ #-}

-- | Coefficient @A@ of E222 curve.
_a :: Fq
_a = 0x1
{-# INLINE _a #-}

-- | Coefficient @D@ of E222 curve.
_d :: Fq
_d = 0x27166
{-# INLINE _d #-}

-- | Cofactor of E222 curve.
_h :: Integer
_h = 0x4
{-# INLINE _h #-}

-- | Characteristic of E222 curve.
_q :: Integer
_q = 0x3fffffffffffffffffffffffffffffffffffffffffffffffffffff8b
{-# INLINE _q #-}

-- | Order of E222 curve.
_r :: Integer
_r = 0xffffffffffffffffffffffffffff70cbc95e932f802f31423598cbf
{-# INLINE _r #-}

-------------------------------------------------------------------------------
-- Affine coordinates
-------------------------------------------------------------------------------

-- | Affine E222 point.
type AP = EAPoint E222 Fq

-- | Affine E222 curve is an Edwards affine curve.
instance EACurve E222 Fq where
  gA_ = gA
  {-# INLINE gA_ #-}
  xA_ = const xA
  {-# INLINE xA_ #-}
  yA_ = const yA
  {-# INLINE yA_ #-}

-- | Generator of affine E222 curve.
gA :: AP
gA = A xA yA
{-# INLINE gA #-}

-- | Coordinate @X@ of affine E222 curve.
xA :: Fq
xA = 0x19b12bb156a389e55c9768c303316d07c23adab3736eb2bc3eb54e51
{-# INLINE xA #-}

-- | Coordinate @Y@ of affine E222 curve.
yA :: Fq
yA = 0x1c
{-# INLINE yA #-}
