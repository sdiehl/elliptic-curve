module Curve.Edwards.E222
  ( module Curve.Edwards
  , module Curve.Edwards.E222
  , Point(..)
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
instance Curve 'Edwards c E222 Fq Fr => ECurve c E222 Fq Fr where
  a_ = const _a
  {-# INLINABLE a_ #-}
  d_ = const _d
  {-# INLINABLE d_ #-}
  h_ = const _h
  {-# INLINABLE h_ #-}
  q_ = const _q
  {-# INLINABLE q_ #-}
  r_ = const _r
  {-# INLINABLE r_ #-}
  x_ = const _x
  {-# INLINABLE x_ #-}
  y_ = const _y
  {-# INLINABLE y_ #-}

-- | Affine E222 curve point.
type PA = EAPoint E222 Fq Fr

-- | Affine E222 curve is an Edwards affine curve.
instance EACurve E222 Fq Fr where
  gA_ = gA
  {-# INLINABLE gA_ #-}

-- | Projective E222 point.
type PP = EPPoint E222 Fq Fr

-- | Projective E222 curve is an Edwards projective curve.
instance EPCurve E222 Fq Fr where
  gP_ = gP
  {-# INLINABLE gP_ #-}

-------------------------------------------------------------------------------
-- Parameters
-------------------------------------------------------------------------------

-- | Coefficient @A@ of E222 curve.
_a :: Fq
_a = 0x1
{-# INLINABLE _a #-}

-- | Coefficient @D@ of E222 curve.
_d :: Fq
_d = 0x27166
{-# INLINABLE _d #-}

-- | Cofactor of E222 curve.
_h :: Integer
_h = 0x4
{-# INLINABLE _h #-}

-- | Characteristic of E222 curve.
_q :: Integer
_q = 0x3fffffffffffffffffffffffffffffffffffffffffffffffffffff8b
{-# INLINABLE _q #-}

-- | Order of E222 curve.
_r :: Integer
_r = 0xffffffffffffffffffffffffffff70cbc95e932f802f31423598cbf
{-# INLINABLE _r #-}

-- | Coordinate @X@ of E222 curve.
_x :: Fq
_x = 0x19b12bb156a389e55c9768c303316d07c23adab3736eb2bc3eb54e51
{-# INLINABLE _x #-}

-- | Coordinate @Y@ of E222 curve.
_y :: Fq
_y = 0x1c
{-# INLINABLE _y #-}

-- | Generator of affine E222 curve.
gA :: PA
gA = A _x _y
{-# INLINABLE gA #-}

-- | Generator of projective E222 curve.
gP :: PP
gP = P _x _y 1
{-# INLINABLE gP #-}
