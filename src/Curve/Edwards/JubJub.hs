module Curve.Edwards.JubJub
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
-- JubJub curve
-------------------------------------------------------------------------------

-- | JubJub curve.
data JubJub

-- | Field of points of JubJub curve.
type Fq = PrimeField 0x73eda753299d7d483339d80809a1d80553bda402fffe5bfeffffffff00000001

-- | Field of coefficients of JubJub curve.
type Fr = PrimeField 0xe7db4ea6533afa906673b0101343b00a6682093ccc81082d0970e5ed6f72cb7

-- | JubJub curve is an Edwards curve.
instance Curve 'Edwards c JubJub Fq => ECurve c JubJub Fq where
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

-------------------------------------------------------------------------------
-- Affine coordinates
-------------------------------------------------------------------------------

-- | Affine JubJub point.
type AP = EAPoint JubJub Fq

-- | Affine JubJub curve is an Edwards affine curve.
instance EACurve JubJub Fq where
  gA_ = gA
  {-# INLINE gA_ #-}
  xA_ = const xA
  {-# INLINE xA_ #-}
  yA_ = const yA
  {-# INLINE yA_ #-}

-- | Generator of affine JubJub curve.
gA :: AP
gA = A xA yA
{-# INLINE gA #-}

-- | Coordinate @X@ of affine JubJub curve.
xA :: Fq
xA = 0x5183972af8eff38ca624b4df00384882000c546bf2f39ede7f4ecf1a74f976c4
{-# INLINE xA #-}

-- | Coordinate @Y@ of affine JubJub curve.
yA :: Fq
yA = 0x3b43f8472ca2fc2c9e8fcc5abd9dc308096c8707ffa6833b146bad709349702e
{-# INLINE yA #-}
