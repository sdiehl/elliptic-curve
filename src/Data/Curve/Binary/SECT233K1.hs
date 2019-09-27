module Data.Curve.Binary.SECT233K1
  ( module Data.Curve.Binary
  , Point(..)
  -- * SECT233K1 curve
  , module Data.Curve.Binary.SECT233K1
  ) where

import Protolude

import Data.Field.Galois
import GHC.Natural (Natural)

import Data.Curve.Binary

-------------------------------------------------------------------------------
-- Types
-------------------------------------------------------------------------------

-- | SECT233K1 curve.
data SECT233K1

-- | Field of points of SECT233K1 curve.
type F2m = Binary P
type P = 0x20000000000000000000000000000000000000004000000000000000001

-- | Field of coefficients of SECT233K1 curve.
type Fr = Prime R
type R = 0x8000000000000000000000000000069d5bb915bcd46efb1ad5f173abdf

-- SECT233K1 curve is a binary curve.
instance Curve 'Binary c SECT233K1 F2m Fr => BCurve c SECT233K1 F2m Fr where
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

-- | Affine SECT233K1 curve point.
type PA = BAPoint SECT233K1 F2m Fr

-- Affine SECT233K1 curve is a binary affine curve.
instance BACurve SECT233K1 F2m Fr where
  gA_ = gA
  {-# INLINABLE gA_ #-}

-- | Projective SECT233K1 point.
type PP = BPPoint SECT233K1 F2m Fr

-- Projective SECT233K1 curve is a binary projective curve.
instance BPCurve SECT233K1 F2m Fr where
  gP_ = gP
  {-# INLINABLE gP_ #-}

-------------------------------------------------------------------------------
-- Parameters
-------------------------------------------------------------------------------

-- | Coefficient @A@ of SECT233K1 curve.
_a :: F2m
_a = 0x0
{-# INLINABLE _a #-}

-- | Coefficient @B@ of SECT233K1 curve.
_b :: F2m
_b = 0x1
{-# INLINABLE _b #-}

-- | Cofactor of SECT233K1 curve.
_h :: Natural
_h = 0x4
{-# INLINABLE _h #-}

-- | Polynomial of SECT233K1 curve.
_p :: Natural
_p = 0x20000000000000000000000000000000000000004000000000000000001
{-# INLINABLE _p #-}

-- | Order of SECT233K1 curve.
_r :: Natural
_r = 0x8000000000000000000000000000069d5bb915bcd46efb1ad5f173abdf
{-# INLINABLE _r #-}

-- | Coordinate @X@ of SECT233K1 curve.
_x :: F2m
_x = 0x17232ba853a7e731af129f22ff4149563a419c26bf50a4c9d6eefad6126
{-# INLINABLE _x #-}

-- | Coordinate @Y@ of SECT233K1 curve.
_y :: F2m
_y = 0x1db537dece819b7f70f555a67c427a8cd9bf18aeb9b56e0c11056fae6a3
{-# INLINABLE _y #-}

-- | Generator of affine SECT233K1 curve.
gA :: PA
gA = A _x _y
{-# INLINABLE gA #-}

-- | Generator of projective SECT233K1 curve.
gP :: PP
gP = P _x _y 1
{-# INLINABLE gP #-}
