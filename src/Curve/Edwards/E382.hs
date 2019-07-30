module Curve.Edwards.E382
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
-- E382 curve
-------------------------------------------------------------------------------

-- | E382 curve.
data E382

-- | Field of points of E382 curve.
type Fq = PrimeField 0x3fffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff97

-- | Field of coefficients of E382 curve.
type Fr = PrimeField 0xfffffffffffffffffffffffffffffffffffffffffffffffd5fb21f21e95eee17c5e69281b102d2773e27e13fd3c9719

-- | E382 curve is an Edwards curve.
instance Curve 'Edwards c E382 Fq => ECurve c E382 Fq where
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

-- | Coefficient @A@ of E382 curve.
_a :: Fq
_a = 0x1
{-# INLINE _a #-}

-- | Coefficient @D@ of E382 curve.
_d :: Fq
_d = 0x3ffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffef8e1
{-# INLINE _d #-}

-- | Cofactor of E382 curve.
_h :: Integer
_h = 0x4
{-# INLINE _h #-}

-- | Characteristic of E382 curve.
_q :: Integer
_q = 0x3fffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff97
{-# INLINE _q #-}

-- | Order of E382 curve.
_r :: Integer
_r = 0xfffffffffffffffffffffffffffffffffffffffffffffffd5fb21f21e95eee17c5e69281b102d2773e27e13fd3c9719
{-# INLINE _r #-}

-------------------------------------------------------------------------------
-- Affine coordinates
-------------------------------------------------------------------------------

-- | Affine E382 point.
type AP = EAPoint E382 Fq

-- | Affine E382 curve is an Edwards affine curve.
instance EACurve E382 Fq where
  gA_ = gA
  {-# INLINE gA_ #-}
  xA_ = const xA
  {-# INLINE xA_ #-}
  yA_ = const yA
  {-# INLINE yA_ #-}

-- | Generator of affine E382 curve.
gA :: AP
gA = A xA yA
{-# INLINE gA #-}

-- | Coordinate @X@ of affine E382 curve.
xA :: Fq
xA = 0x196f8dd0eab20391e5f05be96e8d20ae68f840032b0b64352923bab85364841193517dbce8105398ebc0cc9470f79603
{-# INLINE xA #-}

-- | Coordinate @Y@ of affine E382 curve.
yA :: Fq
yA = 0x11
{-# INLINE yA #-}
