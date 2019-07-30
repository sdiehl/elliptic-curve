module Curve.Edwards.Ed3363
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
-- Ed3363 curve
-------------------------------------------------------------------------------

-- | Ed3363 curve.
data Ed3363

-- | Field of points of Ed3363 curve.
type Fq = PrimeField 0xfffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffd

-- | Field of coefficients of Ed3363 curve.
type Fr = PrimeField 0x200000000000000000000000000000000000000000071415fa9850c0bd6b87f93baa7b2f95973e9fa805

-- | Ed3363 curve is an Edwards curve.
instance Curve 'Edwards c Ed3363 Fq => ECurve c Ed3363 Fq where
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

-- | Coefficient @A@ of Ed3363 curve.
_a :: Fq
_a = 0x1
{-# INLINE _a #-}

-- | Coefficient @D@ of Ed3363 curve.
_d :: Fq
_d = 0x2b67
{-# INLINE _d #-}

-- | Cofactor of Ed3363 curve.
_h :: Integer
_h = 0x8
{-# INLINE _h #-}

-- | Characteristic of Ed3363 curve.
_q :: Integer
_q = 0xfffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffd
{-# INLINE _q #-}

-- | Order of Ed3363 curve.
_r :: Integer
_r = 0x200000000000000000000000000000000000000000071415fa9850c0bd6b87f93baa7b2f95973e9fa805
{-# INLINE _r #-}

-------------------------------------------------------------------------------
-- Affine coordinates
-------------------------------------------------------------------------------

-- | Affine Ed3363 point.
type AP = EAPoint Ed3363 Fq

-- | Affine Ed3363 curve is an Edwards affine curve.
instance EACurve Ed3363 Fq where
  gA_ = gA
  {-# INLINE gA_ #-}
  xA_ = const xA
  {-# INLINE xA_ #-}
  yA_ = const yA
  {-# INLINE yA_ #-}

-- | Generator of affine Ed3363 curve.
gA :: AP
gA = A xA yA
{-# INLINE gA #-}

-- | Coordinate @X@ of affine Ed3363 curve.
xA :: Fq
xA = 0xc
{-# INLINE xA #-}

-- | Coordinate @Y@ of affine Ed3363 curve.
yA :: Fq
yA = 0xc0dc616b56502e18e1c161d007853d1b14b46c3811c7ef435b6db5d5650ca0365db12bec68505fe8632
{-# INLINE yA #-}
