module Data.Curve.Edwards.Curve41417
  ( module Data.Curve.Edwards
  -- * Curve41417 curve
  , module Data.Curve.Edwards.Curve41417
  ) where

import Protolude

import Data.Field.Galois
import GHC.Natural (Natural)

import Data.Curve.Edwards

-------------------------------------------------------------------------------
-- Types
-------------------------------------------------------------------------------

-- | Curve41417 curve.
data Curve41417

-- | Field of points of Curve41417 curve.
type Fq = Prime Q
type Q = 0x3fffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffef

-- | Field of coefficients of Curve41417 curve.
type Fr = Prime R
type R = 0x7ffffffffffffffffffffffffffffffffffffffffffffffffffeb3cc92414cf706022b36f1c0338ad63cf181b0e71a5e106af79

-- Curve41417 curve is an Edwards curve.
instance Curve 'Edwards c Curve41417 Fq Fr => ECurve c Curve41417 Fq Fr where
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

-- | Affine Curve41417 curve point.
type PA = EAPoint Curve41417 Fq Fr

-- Affine Curve41417 curve is an Edwards affine curve.
instance EACurve Curve41417 Fq Fr where
  gA_ = gA
  {-# INLINABLE gA_ #-}

-- | Projective Curve41417 point.
type PP = EPPoint Curve41417 Fq Fr

-- Projective Curve41417 curve is an Edwards projective curve.
instance EPCurve Curve41417 Fq Fr where
  gP_ = gP
  {-# INLINABLE gP_ #-}

-------------------------------------------------------------------------------
-- Parameters
-------------------------------------------------------------------------------

-- | Coefficient @A@ of Curve41417 curve.
_a :: Fq
_a = 0x1
{-# INLINABLE _a #-}

-- | Coefficient @D@ of Curve41417 curve.
_d :: Fq
_d = 0xe21
{-# INLINABLE _d #-}

-- | Cofactor of Curve41417 curve.
_h :: Natural
_h = 0x8
{-# INLINABLE _h #-}

-- | Characteristic of Curve41417 curve.
_q :: Natural
_q = 0x3fffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffef
{-# INLINABLE _q #-}

-- | Order of Curve41417 curve.
_r :: Natural
_r = 0x7ffffffffffffffffffffffffffffffffffffffffffffffffffeb3cc92414cf706022b36f1c0338ad63cf181b0e71a5e106af79
{-# INLINABLE _r #-}

-- | Coordinate @X@ of Curve41417 curve.
_x :: Fq
_x = 0x1a334905141443300218c0631c326e5fcd46369f44c03ec7f57ff35498a4ab4d6d6ba111301a73faa8537c64c4fd3812f3cbc595
{-# INLINABLE _x #-}

-- | Coordinate @Y@ of Curve41417 curve.
_y :: Fq
_y = 0x22
{-# INLINABLE _y #-}

-- | Generator of affine Curve41417 curve.
gA :: PA
gA = A _x _y
{-# INLINABLE gA #-}

-- | Generator of projective Curve41417 curve.
gP :: PP
gP = P _x _y 1
{-# INLINABLE gP #-}
