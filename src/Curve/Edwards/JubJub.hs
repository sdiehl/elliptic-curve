module Curve.Edwards.JubJub
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

-- | JubJub curve.
data JubJub

-- | Field of points of JubJub curve.
type Fq = PrimeField 0x73eda753299d7d483339d80809a1d80553bda402fffe5bfeffffffff00000001

-- | Field of coefficients of JubJub curve.
type Fr = PrimeField 0xe7db4ea6533afa906673b0101343b00a6682093ccc81082d0970e5ed6f72cb7

-- | JubJub curve is an Edwards curve.
instance Curve 'Edwards c JubJub Fq Fr => ECurve c JubJub Fq Fr where
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

-- | Affine JubJub curve point.
type PA = EAPoint JubJub Fq Fr

-- | Affine JubJub curve is an Edwards affine curve.
instance EACurve JubJub Fq Fr where
  gA_ = gA
  {-# INLINE gA_ #-}

-- | Projective JubJub point.
type PP = EPPoint JubJub Fq Fr

-- | Projective JubJub curve is an Edwards projective curve.
instance EPCurve JubJub Fq Fr where
  gP_ = gP
  {-# INLINE gP_ #-}

-------------------------------------------------------------------------------
-- Parameters
-------------------------------------------------------------------------------

-- | Coefficient @A@ of JubJub curve.
_a :: Fq
_a = 0x73eda753299d7d483339d80809a1d80553bda402fffe5bfeffffffff00000000
{-# INLINE _a #-}

-- | Coefficient @D@ of JubJub curve.
_d :: Fq
_d = 0x2a9318e74bfa2b48f5fd9207e6bd7fd4292d7f6d37579d2601065fd6d6343eb1
{-# INLINE _d #-}

-- | Cofactor of JubJub curve.
_h :: Integer
_h = 0x8
{-# INLINE _h #-}

-- | Characteristic of JubJub curve.
_q :: Integer
_q = 0x73eda753299d7d483339d80809a1d80553bda402fffe5bfeffffffff00000001
{-# INLINE _q #-}

-- | Order of JubJub curve.
_r :: Integer
_r = 0xe7db4ea6533afa906673b0101343b00a6682093ccc81082d0970e5ed6f72cb7
{-# INLINE _r #-}

-- | Coordinate @X@ of JubJub curve.
_x :: Fq
_x = 0x5183972af8eff38ca624b4df00384882000c546bf2f39ede7f4ecf1a74f976c4
{-# INLINE _x #-}

-- | Coordinate @Y@ of JubJub curve.
_y :: Fq
_y = 0x3b43f8472ca2fc2c9e8fcc5abd9dc308096c8707ffa6833b146bad709349702e
{-# INLINE _y #-}

-- | Generator of affine JubJub curve.
gA :: PA
gA = A _x _y
{-# INLINE gA #-}

-- | Generator of projective JubJub curve.
gP :: PP
gP = P _x _y 1
{-# INLINE gP #-}
