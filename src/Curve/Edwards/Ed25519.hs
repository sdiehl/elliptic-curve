module Curve.Edwards.Ed25519
  ( module Curve.Edwards
  , module Curve.Edwards.Ed25519
  , Point(..)
  ) where

import Protolude

import Data.Field.Galois

import Curve.Edwards

-------------------------------------------------------------------------------
-- Types
-------------------------------------------------------------------------------

-- | Ed25519 curve.
data Ed25519

-- | Field of points of Ed25519 curve.
type Fq = Prime 0x7fffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffed

-- | Field of coefficients of Ed25519 curve.
type Fr = Prime 0x1000000000000000000000000000000014def9dea2f79cd65812631a5cf5d3ed

-- | Ed25519 curve is an Edwards curve.
instance Curve 'Edwards c Ed25519 Fq Fr => ECurve c Ed25519 Fq Fr where
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

-- | Affine Ed25519 curve point.
type PA = EAPoint Ed25519 Fq Fr

-- | Affine Ed25519 curve is an Edwards affine curve.
instance EACurve Ed25519 Fq Fr where
  gA_ = gA
  {-# INLINABLE gA_ #-}

-- | Projective Ed25519 point.
type PP = EPPoint Ed25519 Fq Fr

-- | Projective Ed25519 curve is an Edwards projective curve.
instance EPCurve Ed25519 Fq Fr where
  gP_ = gP
  {-# INLINABLE gP_ #-}

-------------------------------------------------------------------------------
-- Parameters
-------------------------------------------------------------------------------

-- | Coefficient @A@ of Ed25519 curve.
_a :: Fq
_a = 0x7fffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffec
{-# INLINABLE _a #-}

-- | Coefficient @D@ of Ed25519 curve.
_d :: Fq
_d = 0x52036cee2b6ffe738cc740797779e89800700a4d4141d8ab75eb4dca135978a3
{-# INLINABLE _d #-}

-- | Cofactor of Ed25519 curve.
_h :: Integer
_h = 0x8
{-# INLINABLE _h #-}

-- | Characteristic of Ed25519 curve.
_q :: Integer
_q = 0x7fffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffed
{-# INLINABLE _q #-}

-- | Order of Ed25519 curve.
_r :: Integer
_r = 0x1000000000000000000000000000000014def9dea2f79cd65812631a5cf5d3ed
{-# INLINABLE _r #-}

-- | Coordinate @X@ of Ed25519 curve.
_x :: Fq
_x = 0x216936d3cd6e53fec0a4e231fdd6dc5c692cc7609525a7b2c9562d608f25d51a
{-# INLINABLE _x #-}

-- | Coordinate @Y@ of Ed25519 curve.
_y :: Fq
_y = 0x6666666666666666666666666666666666666666666666666666666666666658
{-# INLINABLE _y #-}

-- | Generator of affine Ed25519 curve.
gA :: PA
gA = A _x _y
{-# INLINABLE gA #-}

-- | Generator of projective Ed25519 curve.
gP :: PP
gP = P _x _y 1
{-# INLINABLE gP #-}
