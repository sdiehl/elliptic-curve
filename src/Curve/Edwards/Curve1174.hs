module Curve.Edwards.Curve1174
  ( module Curve.Edwards
  , module Curve.Edwards.Curve1174
  , Point(..)
  ) where

import Protolude

import PrimeField

import Curve.Edwards

-------------------------------------------------------------------------------
-- Types
-------------------------------------------------------------------------------

-- | Curve1174 curve.
data Curve1174

-- | Field of points of Curve1174 curve.
type Fq = PrimeField 0x7fffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff7

-- | Field of coefficients of Curve1174 curve.
type Fr = PrimeField 0x1fffffffffffffffffffffffffffffff77965c4dfd307348944d45fd166c971

-- | Curve1174 curve is an Edwards curve.
instance Curve 'Edwards c Curve1174 Fq Fr => ECurve c Curve1174 Fq Fr where
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

-- | Affine Curve1174 curve point.
type PA = EAPoint Curve1174 Fq Fr

-- | Affine Curve1174 curve is an Edwards affine curve.
instance EACurve Curve1174 Fq Fr where
  gA_ = gA
  {-# INLINABLE gA_ #-}

-- | Projective Curve1174 point.
type PP = EPPoint Curve1174 Fq Fr

-- | Projective Curve1174 curve is an Edwards projective curve.
instance EPCurve Curve1174 Fq Fr where
  gP_ = gP
  {-# INLINABLE gP_ #-}

-------------------------------------------------------------------------------
-- Parameters
-------------------------------------------------------------------------------

-- | Coefficient @A@ of Curve1174 curve.
_a :: Fq
_a = 0x1
{-# INLINABLE _a #-}

-- | Coefficient @D@ of Curve1174 curve.
_d :: Fq
_d = 0x7fffffffffffffffffffffffffffffffffffffffffffffffffffffffffffb61
{-# INLINABLE _d #-}

-- | Cofactor of Curve1174 curve.
_h :: Integer
_h = 0x4
{-# INLINABLE _h #-}

-- | Characteristic of Curve1174 curve.
_q :: Integer
_q = 0x7fffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff7
{-# INLINABLE _q #-}

-- | Order of Curve1174 curve.
_r :: Integer
_r = 0x1fffffffffffffffffffffffffffffff77965c4dfd307348944d45fd166c971
{-# INLINABLE _r #-}

-- | Coordinate @X@ of Curve1174 curve.
_x :: Fq
_x = 0x37fbb0cea308c479343aee7c029a190c021d96a492ecd6516123f27bce29eda
{-# INLINABLE _x #-}

-- | Coordinate @Y@ of Curve1174 curve.
_y :: Fq
_y = 0x6b72f82d47fb7cc6656841169840e0c4fe2dee2af3f976ba4ccb1bf9b46360e
{-# INLINABLE _y #-}

-- | Generator of affine Curve1174 curve.
gA :: PA
gA = A _x _y
{-# INLINABLE gA #-}

-- | Generator of projective Curve1174 curve.
gP :: PP
gP = P _x _y 1
{-# INLINABLE gP #-}
