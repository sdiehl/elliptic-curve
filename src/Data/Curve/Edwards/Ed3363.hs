module Data.Curve.Edwards.Ed3363
  ( module Data.Curve.Edwards
  , module Data.Curve.Edwards.Ed3363
  , Point(..)
  ) where

import Protolude

import Data.Field.Galois

import Data.Curve.Edwards

-------------------------------------------------------------------------------
-- Types
-------------------------------------------------------------------------------

-- | Ed3363 curve.
data Ed3363

-- | Field of points of Ed3363 curve.
type Fq = Prime 0xfffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffd

-- | Field of coefficients of Ed3363 curve.
type Fr = Prime 0x200000000000000000000000000000000000000000071415fa9850c0bd6b87f93baa7b2f95973e9fa805

-- | Ed3363 curve is an Edwards curve.
instance Curve 'Edwards c Ed3363 Fq Fr => ECurve c Ed3363 Fq Fr where
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

-- | Affine Ed3363 curve point.
type PA = EAPoint Ed3363 Fq Fr

-- | Affine Ed3363 curve is an Edwards affine curve.
instance EACurve Ed3363 Fq Fr where
  gA_ = gA
  {-# INLINABLE gA_ #-}

-- | Projective Ed3363 point.
type PP = EPPoint Ed3363 Fq Fr

-- | Projective Ed3363 curve is an Edwards projective curve.
instance EPCurve Ed3363 Fq Fr where
  gP_ = gP
  {-# INLINABLE gP_ #-}

-------------------------------------------------------------------------------
-- Parameters
-------------------------------------------------------------------------------

-- | Coefficient @A@ of Ed3363 curve.
_a :: Fq
_a = 0x1
{-# INLINABLE _a #-}

-- | Coefficient @D@ of Ed3363 curve.
_d :: Fq
_d = 0x2b67
{-# INLINABLE _d #-}

-- | Cofactor of Ed3363 curve.
_h :: Natural
_h = 0x8
{-# INLINABLE _h #-}

-- | Characteristic of Ed3363 curve.
_q :: Natural
_q = 0xfffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffd
{-# INLINABLE _q #-}

-- | Order of Ed3363 curve.
_r :: Natural
_r = 0x200000000000000000000000000000000000000000071415fa9850c0bd6b87f93baa7b2f95973e9fa805
{-# INLINABLE _r #-}

-- | Coordinate @X@ of Ed3363 curve.
_x :: Fq
_x = 0xc
{-# INLINABLE _x #-}

-- | Coordinate @Y@ of Ed3363 curve.
_y :: Fq
_y = 0xc0dc616b56502e18e1c161d007853d1b14b46c3811c7ef435b6db5d5650ca0365db12bec68505fe8632
{-# INLINABLE _y #-}

-- | Generator of affine Ed3363 curve.
gA :: PA
gA = A _x _y
{-# INLINABLE gA #-}

-- | Generator of projective Ed3363 curve.
gP :: PP
gP = P _x _y 1
{-# INLINABLE gP #-}
