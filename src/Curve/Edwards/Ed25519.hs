module Curve.Edwards.Ed25519
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
  , gA
  , gP
  ) where

import Protolude

import PrimeField

import Curve.Edwards

-------------------------------------------------------------------------------
-- Types
-------------------------------------------------------------------------------

-- | Ed25519 curve.
data Ed25519

-- | Field of points of Ed25519 curve.
type Fq = PrimeField 0x7fffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffed

-- | Field of coefficients of Ed25519 curve.
type Fr = PrimeField 0x1000000000000000000000000000000014def9dea2f79cd65812631a5cf5d3ed

-- | Ed25519 curve is an Edwards curve.
instance Curve 'Edwards c Ed25519 Fq Fr => ECurve c Ed25519 Fq Fr where
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

-- | Affine Ed25519 curve point.
type PA = EAPoint Ed25519 Fq Fr

-- | Affine Ed25519 curve is an Edwards affine curve.
instance EACurve Ed25519 Fq Fr where
  gA_ = gA
  {-# INLINE gA_ #-}

-- | Projective Ed25519 point.
type PP = EPPoint Ed25519 Fq Fr

-- | Projective Ed25519 curve is an Edwards projective curve.
instance EPCurve Ed25519 Fq Fr where
  gP_ = gP
  {-# INLINE gP_ #-}

-------------------------------------------------------------------------------
-- Parameters
-------------------------------------------------------------------------------

-- | Coefficient @A@ of Ed25519 curve.
_a :: Fq
_a = 0x7fffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffec
{-# INLINE _a #-}

-- | Coefficient @D@ of Ed25519 curve.
_d :: Fq
_d = 0x52036cee2b6ffe738cc740797779e89800700a4d4141d8ab75eb4dca135978a3
{-# INLINE _d #-}

-- | Cofactor of Ed25519 curve.
_h :: Integer
_h = 0x8
{-# INLINE _h #-}

-- | Characteristic of Ed25519 curve.
_q :: Integer
_q = 0x7fffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffed
{-# INLINE _q #-}

-- | Order of Ed25519 curve.
_r :: Integer
_r = 0x1000000000000000000000000000000014def9dea2f79cd65812631a5cf5d3ed
{-# INLINE _r #-}

-- | Coordinate @X@ of Ed25519 curve.
_x :: Fq
_x = 0x216936d3cd6e53fec0a4e231fdd6dc5c692cc7609525a7b2c9562d608f25d51a
{-# INLINE _x #-}

-- | Coordinate @Y@ of Ed25519 curve.
_y :: Fq
_y = 0x6666666666666666666666666666666666666666666666666666666666666658
{-# INLINE _y #-}

-- | Generator of affine Ed25519 curve.
gA :: PA
gA = A _x _y
{-# INLINE gA #-}

-- | Generator of projective Ed25519 curve.
gP :: PP
gP = P _x _y 1
{-# INLINE gP #-}
