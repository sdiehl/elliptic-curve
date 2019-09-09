module Data.Curve.Edwards.Ed448
  ( module Data.Curve.Edwards
  -- * Ed448 curve
  , module Data.Curve.Edwards.Ed448
  ) where

import Protolude

import Data.Field.Galois
import GHC.Natural (Natural)

import Data.Curve.Edwards

-------------------------------------------------------------------------------
-- Types
-------------------------------------------------------------------------------

-- | Ed448 curve.
data Ed448

-- | Field of points of Ed448 curve.
type Fq = Prime Q
type Q = 0xfffffffffffffffffffffffffffffffffffffffffffffffffffffffeffffffffffffffffffffffffffffffffffffffffffffffffffffffff

-- | Field of coefficients of Ed448 curve.
type Fr = Prime R
type R = 0x3fffffffffffffffffffffffffffffffffffffffffffffffffffffff7cca23e9c44edb49aed63690216cc2728dc58f552378c292ab5844f3

-- Ed448 curve is an Edwards curve.
instance Curve 'Edwards c Ed448 Fq Fr => ECurve c Ed448 Fq Fr where
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

-- | Affine Ed448 curve point.
type PA = EAPoint Ed448 Fq Fr

-- Affine Ed448 curve is an Edwards affine curve.
instance EACurve Ed448 Fq Fr where
  gA_ = gA
  {-# INLINABLE gA_ #-}

-- | Projective Ed448 point.
type PP = EPPoint Ed448 Fq Fr

-- Projective Ed448 curve is an Edwards projective curve.
instance EPCurve Ed448 Fq Fr where
  gP_ = gP
  {-# INLINABLE gP_ #-}

-------------------------------------------------------------------------------
-- Parameters
-------------------------------------------------------------------------------

-- | Coefficient @A@ of Ed448 curve.
_a :: Fq
_a = 0x1
{-# INLINABLE _a #-}

-- | Coefficient @D@ of Ed448 curve.
_d :: Fq
_d = 0xfffffffffffffffffffffffffffffffffffffffffffffffffffffffeffffffffffffffffffffffffffffffffffffffffffffffffffff6756
{-# INLINABLE _d #-}

-- | Cofactor of Ed448 curve.
_h :: Natural
_h = 0x4
{-# INLINABLE _h #-}

-- | Characteristic of Ed448 curve.
_q :: Natural
_q = 0xfffffffffffffffffffffffffffffffffffffffffffffffffffffffeffffffffffffffffffffffffffffffffffffffffffffffffffffffff
{-# INLINABLE _q #-}

-- | Order of Ed448 curve.
_r :: Natural
_r = 0x3fffffffffffffffffffffffffffffffffffffffffffffffffffffff7cca23e9c44edb49aed63690216cc2728dc58f552378c292ab5844f3
{-# INLINABLE _r #-}

-- | Coordinate @X@ of Ed448 curve.
_x :: Fq
_x = 0x297ea0ea2692ff1b4faff46098453a6a26adf733245f065c3c59d0709cecfa96147eaaf3932d94c63d96c170033f4ba0c7f0de840aed939f
{-# INLINABLE _x #-}

-- | Coordinate @Y@ of Ed448 curve.
_y :: Fq
_y = 0x13
{-# INLINABLE _y #-}

-- | Generator of affine Ed448 curve.
gA :: PA
gA = A _x _y
{-# INLINABLE gA #-}

-- | Generator of projective Ed448 curve.
gP :: PP
gP = P _x _y 1
{-# INLINABLE gP #-}
