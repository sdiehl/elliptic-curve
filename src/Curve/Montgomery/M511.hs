module Curve.Montgomery.M511
  ( AP
  , Curve(..)
  , Fq
  , Fr
  , Group(..)
  , MCurve(..)
  , MPoint
  , MACurve(..)
  , MAPoint
  , Point(..)
  , _a
  , _b
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
import Curve.Montgomery (MCurve(..), MPoint, MACurve(..), MAPoint, Point(..))
import Group (Group(..))

-------------------------------------------------------------------------------
-- M511 curve
-------------------------------------------------------------------------------

-- | M511 curve.
data M511

-- | Field of points of M511 curve.
type Fq = PrimeField 0x7fffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff45

-- | Field of coefficients of M511 curve.
type Fr = PrimeField 0x100000000000000000000000000000000000000000000000000000000000000017b5feff30c7f5677ab2aeebd13779a2ac125042a6aa10bfa54c15bab76baf1b

-- | M511 curve is a Montgomery curve.
instance Curve 'Montgomery c M511 Fq => MCurve c M511 Fq where
  a_ = const _a
  {-# INLINE a_ #-}
  b_ = const _b
  {-# INLINE b_ #-}
  h_ = const _h
  {-# INLINE h_ #-}
  q_ = const _q
  {-# INLINE q_ #-}
  r_ = const _r
  {-# INLINE r_ #-}

-- | Coefficient @A@ of M511 curve.
_a :: Fq
_a = 0x81806
{-# INLINE _a #-}

-- | Coefficient @B@ of M511 curve.
_b :: Fq
_b = 0x1
{-# INLINE _b #-}

-- | Cofactor of M511 curve.
_h :: Integer
_h = 0x8
{-# INLINE _h #-}

-- | Characteristic of M511 curve.
_q :: Integer
_q = 0x7fffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff45
{-# INLINE _q #-}

-- | Order of M511 curve.
_r :: Integer
_r = 0x100000000000000000000000000000000000000000000000000000000000000017b5feff30c7f5677ab2aeebd13779a2ac125042a6aa10bfa54c15bab76baf1b
{-# INLINE _r #-}

-------------------------------------------------------------------------------
-- Affine coordinates
-------------------------------------------------------------------------------

-- | Affine M511 point.
type AP = MAPoint M511 Fq

-- | Affine M511 curve is a Montgomery affine curve.
instance MACurve M511 Fq where
  gA_ = gA
  {-# INLINE gA_ #-}
  xA_ = const xA
  {-# INLINE xA_ #-}
  yA_ = const yA
  {-# INLINE yA_ #-}

-- | Generator of affine M511 curve.
gA :: AP
gA = A xA yA
{-# INLINE gA #-}

-- | Coordinate @X@ of affine M511 curve.
xA :: Fq
xA = 0x5
{-# INLINE xA #-}

-- | Coordinate @Y@ of affine M511 curve.
yA :: Fq
yA = 0x2fbdc0ad8530803d28fdbad354bb488d32399ac1cf8f6e01ee3f96389b90c809422b9429e8a43dbf49308ac4455940abe9f1dbca542093a895e30a64af056fa5
{-# INLINE yA #-}
