module Curve.Edwards.E382
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
  x_ = const _x
  {-# INLINE x_ #-}
  y_ = const _y
  {-# INLINE y_ #-}

-- | Affine E382 curve point.
type PA = EAPoint E382 Fq

-- | Affine E382 curve is an Edwards affine curve.
instance EACurve E382 Fq where
  gA_ = gA
  {-# INLINE gA_ #-}

-- | Projective E382 point.
type PP = EPPoint E382 Fq

-- | Projective E382 curve is an Edwards projective curve.
instance EPCurve E382 Fq where
  gP_ = gP
  {-# INLINE gP_ #-}

-------------------------------------------------------------------------------
-- Parameters
-------------------------------------------------------------------------------

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

-- | Coordinate @X@ of E382 curve.
_x :: Fq
_x = 0x196f8dd0eab20391e5f05be96e8d20ae68f840032b0b64352923bab85364841193517dbce8105398ebc0cc9470f79603
{-# INLINE _x #-}

-- | Coordinate @Y@ of E382 curve.
_y :: Fq
_y = 0x11
{-# INLINE _y #-}

-- | Generator of affine E382 curve.
gA :: PA
gA = fromMaybe (panic "not well-defined.") (point _x _y)
{-# INLINE gA #-}

-- | Generator of projective E382 curve.
gP :: PP
gP = fromMaybe (panic "not well-defined.") (point _x _y)
{-# INLINE gP #-}
