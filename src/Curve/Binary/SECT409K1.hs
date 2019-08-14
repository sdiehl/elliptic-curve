module Curve.Binary.SECT409K1
  ( module Curve.Binary
  , module Curve.Binary.SECT409K1
  , Point(..)
  ) where

import Protolude

import BinaryField
import PrimeField

import Curve.Binary

-------------------------------------------------------------------------------
-- Types
-------------------------------------------------------------------------------

-- | SECT409K1 curve.
data SECT409K1

-- | Field of points of SECT409K1 curve.
type F2m = BinaryField 0x2000000000000000000000000000000000000000000000000000000000000000000000000000000008000000000000000000001

-- | Field of coefficients of SECT409K1 curve.
type Fr = PrimeField 0x7ffffffffffffffffffffffffffffffffffffffffffffffffffe5f83b2d4ea20400ec4557d5ed3e3e7ca5b4b5c83b8e01e5fcf

-- | SECT409K1 curve is a binary curve.
instance Curve 'Binary c SECT409K1 F2m Fr => BCurve c SECT409K1 F2m Fr where
  a_ = const _a
  {-# INLINABLE a_ #-}
  b_ = const _b
  {-# INLINABLE b_ #-}
  h_ = const _h
  {-# INLINABLE h_ #-}
  p_ = const _p
  {-# INLINABLE p_ #-}
  r_ = const _r
  {-# INLINABLE r_ #-}
  x_ = const _x
  {-# INLINABLE x_ #-}
  y_ = const _y
  {-# INLINABLE y_ #-}

-- | Affine SECT409K1 curve point.
type PA = BAPoint SECT409K1 F2m Fr

-- | Affine SECT409K1 curve is a binary affine curve.
instance BACurve SECT409K1 F2m Fr where
  gA_ = gA
  {-# INLINABLE gA_ #-}

-- | Projective SECT409K1 point.
type PP = BPPoint SECT409K1 F2m Fr

-- | Projective SECT409K1 curve is a binary projective curve.
instance BPCurve SECT409K1 F2m Fr where
  gP_ = gP
  {-# INLINABLE gP_ #-}

-------------------------------------------------------------------------------
-- Parameters
-------------------------------------------------------------------------------

-- | Coefficient @A@ of SECT409K1 curve.
_a :: F2m
_a = 0x0
{-# INLINABLE _a #-}

-- | Coefficient @B@ of SECT409K1 curve.
_b :: F2m
_b = 0x1
{-# INLINABLE _b #-}

-- | Cofactor of SECT409K1 curve.
_h :: Integer
_h = 0x4
{-# INLINABLE _h #-}

-- | Polynomial of SECT409K1 curve.
_p :: Integer
_p = 0x2000000000000000000000000000000000000000000000000000000000000000000000000000000008000000000000000000001
{-# INLINABLE _p #-}

-- | Order of SECT409K1 curve.
_r :: Integer
_r = 0x7ffffffffffffffffffffffffffffffffffffffffffffffffffe5f83b2d4ea20400ec4557d5ed3e3e7ca5b4b5c83b8e01e5fcf
{-# INLINABLE _r #-}

-- | Coordinate @X@ of SECT409K1 curve.
_x :: F2m
_x = 0x60f05f658f49c1ad3ab1890f7184210efd0987e307c84c27accfb8f9f67cc2c460189eb5aaaa62ee222eb1b35540cfe9023746
{-# INLINABLE _x #-}

-- | Coordinate @Y@ of SECT409K1 curve.
_y :: F2m
_y = 0x1e369050b7c4e42acba1dacbf04299c3460782f918ea427e6325165e9ea10e3da5f6c42e9c55215aa9ca27a5863ec48d8e0286b
{-# INLINABLE _y #-}

-- | Generator of affine SECT409K1 curve.
gA :: PA
gA = A _x _y
{-# INLINABLE gA #-}

-- | Generator of projective SECT409K1 curve.
gP :: PP
gP = P _x _y 1
{-# INLINABLE gP #-}
