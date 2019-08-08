module Curve.Edwards.Curve41417
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

-- | Curve41417 curve.
data Curve41417

-- | Field of points of Curve41417 curve.
type Fq = PrimeField 0x3fffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffef

-- | Field of coefficients of Curve41417 curve.
type Fr = PrimeField 0x7ffffffffffffffffffffffffffffffffffffffffffffffffffeb3cc92414cf706022b36f1c0338ad63cf181b0e71a5e106af79

-- | Curve41417 curve is an Edwards curve.
instance Curve 'Edwards c Curve41417 Fq Fr => ECurve c Curve41417 Fq Fr where
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

-- | Affine Curve41417 curve point.
type PA = EAPoint Curve41417 Fq Fr

-- | Affine Curve41417 curve is an Edwards affine curve.
instance EACurve Curve41417 Fq Fr where
  gA_ = gA
  {-# INLINE gA_ #-}

-- | Projective Curve41417 point.
type PP = EPPoint Curve41417 Fq Fr

-- | Projective Curve41417 curve is an Edwards projective curve.
instance EPCurve Curve41417 Fq Fr where
  gP_ = gP
  {-# INLINE gP_ #-}

-------------------------------------------------------------------------------
-- Parameters
-------------------------------------------------------------------------------

-- | Coefficient @A@ of Curve41417 curve.
_a :: Fq
_a = 0x1
{-# INLINE _a #-}

-- | Coefficient @D@ of Curve41417 curve.
_d :: Fq
_d = 0xe21
{-# INLINE _d #-}

-- | Cofactor of Curve41417 curve.
_h :: Integer
_h = 0x8
{-# INLINE _h #-}

-- | Characteristic of Curve41417 curve.
_q :: Integer
_q = 0x3fffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffef
{-# INLINE _q #-}

-- | Order of Curve41417 curve.
_r :: Integer
_r = 0x7ffffffffffffffffffffffffffffffffffffffffffffffffffeb3cc92414cf706022b36f1c0338ad63cf181b0e71a5e106af79
{-# INLINE _r #-}

-- | Coordinate @X@ of Curve41417 curve.
_x :: Fq
_x = 0x1a334905141443300218c0631c326e5fcd46369f44c03ec7f57ff35498a4ab4d6d6ba111301a73faa8537c64c4fd3812f3cbc595
{-# INLINE _x #-}

-- | Coordinate @Y@ of Curve41417 curve.
_y :: Fq
_y = 0x22
{-# INLINE _y #-}

-- | Generator of affine Curve41417 curve.
gA :: PA
gA = A _x _y
{-# INLINE gA #-}

-- | Generator of projective Curve41417 curve.
gP :: PP
gP = P _x _y 1
{-# INLINE gP #-}
