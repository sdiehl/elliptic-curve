module Curve.Edwards.E222
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
  x_ = const _x
  {-# INLINE x_ #-}
  y_ = const _y
  {-# INLINE y_ #-}

-- | Affine E222 curve point.
type PA = EAPoint E222 Fq

-- | Affine E222 curve is an Edwards affine curve.
instance EACurve E222 Fq where
  gA_ = gA
  {-# INLINE gA_ #-}

-- | Projective E222 point.
type PP = EPPoint E222 Fq

-- | Projective E222 curve is an Edwards projective curve.
instance EPCurve E222 Fq where
  gP_ = gP
  {-# INLINE gP_ #-}

-------------------------------------------------------------------------------
-- Parameters
-------------------------------------------------------------------------------

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

-- | Coordinate @X@ of E222 curve.
_x :: Fq
_x = 0x19b12bb156a389e55c9768c303316d07c23adab3736eb2bc3eb54e51
{-# INLINE _x #-}

-- | Coordinate @Y@ of E222 curve.
_y :: Fq
_y = 0x1c
{-# INLINE _y #-}

-- | Generator of affine E222 curve.
gA :: PA
gA = A _x _y
{-# INLINE gA #-}

-- | Generator of projective E222 curve.
gP :: PP
gP = P _x _y 1
{-# INLINE gP #-}
