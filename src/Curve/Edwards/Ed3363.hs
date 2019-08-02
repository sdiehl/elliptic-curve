module Curve.Edwards.Ed3363
  ( Curve(..)
  , ECurve(..)
  , EPoint
  , EACurve(..)
  , EAPoint
  , EPCurve(..)
  , EPPoint
  , Fq
  , Fr
  , Group(..)
  , PA
  , PP
  , Point(..)
  , _a
  , _d
  , _h
  , _q
  , _r
  , _x
  , _y
  , fromAtoP
  , fromPtoA
  , gA
  , gP
  ) where

import Protolude

import PrimeField

import Curve.Edwards

-------------------------------------------------------------------------------
-- Types
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
  x_ = const _x
  {-# INLINE x_ #-}
  y_ = const _y
  {-# INLINE y_ #-}

-- | Affine Ed3363 curve point.
type PA = EAPoint Ed3363 Fq

-- | Affine Ed3363 curve is an Edwards affine curve.
instance EACurve Ed3363 Fq where
  gA_ = gA
  {-# INLINE gA_ #-}

-- | Projective Ed3363 point.
type PP = EPPoint Ed3363 Fq

-- | Projective Ed3363 curve is an Edwards projective curve.
instance EPCurve Ed3363 Fq where
  gP_ = gP
  {-# INLINE gP_ #-}

-------------------------------------------------------------------------------
-- Parameters
-------------------------------------------------------------------------------

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

-- | Coordinate @X@ of Ed3363 curve.
_x :: Fq
_x = 0xc
{-# INLINE _x #-}

-- | Coordinate @Y@ of Ed3363 curve.
_y :: Fq
_y = 0xc0dc616b56502e18e1c161d007853d1b14b46c3811c7ef435b6db5d5650ca0365db12bec68505fe8632
{-# INLINE _y #-}

-- | Generator of affine Ed3363 curve.
gA :: PA
gA = A _x _y
{-# INLINE gA #-}

-- | Generator of projective Ed3363 curve.
gP :: PP
gP = P _x _y 1
{-# INLINE gP #-}
