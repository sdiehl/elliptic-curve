module Data.Curve.Edwards.E521
  ( module Data.Curve.Edwards
  , module Data.Curve.Edwards.E521
  , Point(..)
  ) where

import Protolude

import Data.Field.Galois

import Data.Curve.Edwards

-------------------------------------------------------------------------------
-- Types
-------------------------------------------------------------------------------

-- | E521 curve.
data E521

-- | Field of points of E521 curve.
type Fq = Prime 0x1ffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff

-- | Field of coefficients of E521 curve.
type Fr = Prime 0x7ffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffd15b6c64746fc85f736b8af5e7ec53f04fbd8c4569a8f1f4540ea2435f5180d6b

-- | E521 curve is an Edwards curve.
instance Curve 'Edwards c E521 Fq Fr => ECurve c E521 Fq Fr where
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

-- | Affine E521 curve point.
type PA = EAPoint E521 Fq Fr

-- | Affine E521 curve is an Edwards affine curve.
instance EACurve E521 Fq Fr where
  gA_ = gA
  {-# INLINABLE gA_ #-}

-- | Projective E521 point.
type PP = EPPoint E521 Fq Fr

-- | Projective E521 curve is an Edwards projective curve.
instance EPCurve E521 Fq Fr where
  gP_ = gP
  {-# INLINABLE gP_ #-}

-------------------------------------------------------------------------------
-- Parameters
-------------------------------------------------------------------------------

-- | Coefficient @A@ of E521 curve.
_a :: Fq
_a = 0x1
{-# INLINABLE _a #-}

-- | Coefficient @D@ of E521 curve.
_d :: Fq
_d = 0x1fffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffa4331
{-# INLINABLE _d #-}

-- | Cofactor of E521 curve.
_h :: Integer
_h = 0x4
{-# INLINABLE _h #-}

-- | Characteristic of E521 curve.
_q :: Integer
_q = 0x1ffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff
{-# INLINABLE _q #-}

-- | Order of E521 curve.
_r :: Integer
_r = 0x7ffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffd15b6c64746fc85f736b8af5e7ec53f04fbd8c4569a8f1f4540ea2435f5180d6b
{-# INLINABLE _r #-}

-- | Coordinate @X@ of E521 curve.
_x :: Fq
_x = 0x752cb45c48648b189df90cb2296b2878a3bfd9f42fc6c818ec8bf3c9c0c6203913f6ecc5ccc72434b1ae949d568fc99c6059d0fb13364838aa302a940a2f19ba6c
{-# INLINABLE _x #-}

-- | Coordinate @Y@ of E521 curve.
_y :: Fq
_y = 0xc
{-# INLINABLE _y #-}

-- | Generator of affine E521 curve.
gA :: PA
gA = A _x _y
{-# INLINABLE gA #-}

-- | Generator of projective E521 curve.
gP :: PP
gP = P _x _y 1
{-# INLINABLE gP #-}
