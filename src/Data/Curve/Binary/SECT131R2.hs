module Data.Curve.Binary.SECT131R2
  ( module Data.Curve.Binary
  -- * SECT131R2 curve
  , module Data.Curve.Binary.SECT131R2
  ) where

import Protolude

import Data.Field.Galois
import GHC.Natural (Natural)

import Data.Curve.Binary

-------------------------------------------------------------------------------
-- Types
-------------------------------------------------------------------------------

-- | SECT131R2 curve.
data SECT131R2

-- | Field of points of SECT131R2 curve.
type F2m = Binary M
type M = 0x80000000000000000000000000000010d

-- | Field of coefficients of SECT131R2 curve.
type Fr = Prime R
type R = 0x400000000000000016954a233049ba98f

-- SECT131R2 curve is a binary curve.
instance Curve 'Binary c SECT131R2 F2m Fr => BCurve c SECT131R2 F2m Fr where
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

-- | Affine SECT131R2 curve point.
type PA = BAPoint SECT131R2 F2m Fr

-- Affine SECT131R2 curve is a binary affine curve.
instance BACurve SECT131R2 F2m Fr where
  gA_ = gA
  {-# INLINABLE gA_ #-}

-- | Projective SECT131R2 point.
type PP = BPPoint SECT131R2 F2m Fr

-- Projective SECT131R2 curve is a binary projective curve.
instance BPCurve SECT131R2 F2m Fr where
  gP_ = gP
  {-# INLINABLE gP_ #-}

-------------------------------------------------------------------------------
-- Parameters
-------------------------------------------------------------------------------

-- | Coefficient @A@ of SECT131R2 curve.
_a :: F2m
_a = 0x3e5a88919d7cafcbf415f07c2176573b2
{-# INLINABLE _a #-}

-- | Coefficient @B@ of SECT131R2 curve.
_b :: F2m
_b = 0x4b8266a46c55657ac734ce38f018f2192
{-# INLINABLE _b #-}

-- | Cofactor of SECT131R2 curve.
_h :: Natural
_h = 0x2
{-# INLINABLE _h #-}

-- | Polynomial of SECT131R2 curve.
_p :: Natural
_p = 0x80000000000000000000000000000010d
{-# INLINABLE _p #-}

-- | Order of SECT131R2 curve.
_r :: Natural
_r = 0x400000000000000016954a233049ba98f
{-# INLINABLE _r #-}

-- | Coordinate @X@ of SECT131R2 curve.
_x :: F2m
_x = 0x356dcd8f2f95031ad652d23951bb366a8
{-# INLINABLE _x #-}

-- | Coordinate @Y@ of SECT131R2 curve.
_y :: F2m
_y = 0x648f06d867940a5366d9e265de9eb240f
{-# INLINABLE _y #-}

-- | Generator of affine SECT131R2 curve.
gA :: PA
gA = A _x _y
{-# INLINABLE gA #-}

-- | Generator of projective SECT131R2 curve.
gP :: PP
gP = P _x _y 1
{-# INLINABLE gP #-}
