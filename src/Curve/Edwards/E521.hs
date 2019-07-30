module Curve.Edwards.E521
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
-- E521 curve
-------------------------------------------------------------------------------

-- | E521 curve.
data E521

-- | Field of points of E521 curve.
type Fq = PrimeField 0x1ffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff

-- | Field of coefficients of E521 curve.
type Fr = PrimeField 0x7ffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffd15b6c64746fc85f736b8af5e7ec53f04fbd8c4569a8f1f4540ea2435f5180d6b

-- | E521 curve is an Edwards curve.
instance Curve 'Edwards c E521 Fq => ECurve c E521 Fq where
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

-- | Coefficient @A@ of E521 curve.
_a :: Fq
_a = 0x1
{-# INLINE _a #-}

-- | Coefficient @D@ of E521 curve.
_d :: Fq
_d = 0x1fffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffa4331
{-# INLINE _d #-}

-- | Cofactor of E521 curve.
_h :: Integer
_h = 0x4
{-# INLINE _h #-}

-- | Characteristic of E521 curve.
_q :: Integer
_q = 0x1ffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff
{-# INLINE _q #-}

-- | Order of E521 curve.
_r :: Integer
_r = 0x7ffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffd15b6c64746fc85f736b8af5e7ec53f04fbd8c4569a8f1f4540ea2435f5180d6b
{-# INLINE _r #-}

-------------------------------------------------------------------------------
-- Affine coordinates
-------------------------------------------------------------------------------

-- | Affine E521 point.
type AP = EAPoint E521 Fq

-- | Affine E521 curve is an Edwards affine curve.
instance EACurve E521 Fq where
  gA_ = gA
  {-# INLINE gA_ #-}
  xA_ = const xA
  {-# INLINE xA_ #-}
  yA_ = const yA
  {-# INLINE yA_ #-}

-- | Generator of affine E521 curve.
gA :: AP
gA = A xA yA
{-# INLINE gA #-}

-- | Coordinate @X@ of affine E521 curve.
xA :: Fq
xA = 0x752cb45c48648b189df90cb2296b2878a3bfd9f42fc6c818ec8bf3c9c0c6203913f6ecc5ccc72434b1ae949d568fc99c6059d0fb13364838aa302a940a2f19ba6c
{-# INLINE xA #-}

-- | Coordinate @Y@ of affine E521 curve.
yA :: Fq
yA = 0xc
{-# INLINE yA #-}
