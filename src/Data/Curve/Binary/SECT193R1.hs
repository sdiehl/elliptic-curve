module Data.Curve.Binary.SECT193R1
  ( module Data.Curve.Binary
  -- * SECT193R1 curve
  , module Data.Curve.Binary.SECT193R1
  ) where

import Protolude

import Data.Field.Galois
import GHC.Natural (Natural)

import Data.Curve.Binary

-------------------------------------------------------------------------------
-- Types
-------------------------------------------------------------------------------

-- | SECT193R1 curve.
data SECT193R1

-- | Field of points of SECT193R1 curve.
type F2m = Binary P
type P = 0x2000000000000000000000000000000000000000000008001

-- | Field of coefficients of SECT193R1 curve.
type Fr = Prime R
type R = 0x1000000000000000000000000c7f34a778f443acc920eba49

-- SECT193R1 curve is a binary curve.
instance Curve 'Binary c SECT193R1 F2m Fr => BCurve c SECT193R1 F2m Fr where
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

-- | Affine SECT193R1 curve point.
type PA = BAPoint SECT193R1 F2m Fr

-- Affine SECT193R1 curve is a binary affine curve.
instance BACurve SECT193R1 F2m Fr where
  gA_ = gA
  {-# INLINABLE gA_ #-}

-- | Projective SECT193R1 point.
type PP = BPPoint SECT193R1 F2m Fr

-- Projective SECT193R1 curve is a binary projective curve.
instance BPCurve SECT193R1 F2m Fr where
  gP_ = gP
  {-# INLINABLE gP_ #-}

-------------------------------------------------------------------------------
-- Parameters
-------------------------------------------------------------------------------

-- | Coefficient @A@ of SECT193R1 curve.
_a :: F2m
_a = 0x17858feb7a98975169e171f77b4087de098ac8a911df7b01
{-# INLINABLE _a #-}

-- | Coefficient @B@ of SECT193R1 curve.
_b :: F2m
_b = 0xfdfb49bfe6c3a89facadaa7a1e5bbc7cc1c2e5d831478814
{-# INLINABLE _b #-}

-- | Cofactor of SECT193R1 curve.
_h :: Natural
_h = 0x2
{-# INLINABLE _h #-}

-- | Polynomial of SECT193R1 curve.
_p :: Natural
_p = 0x2000000000000000000000000000000000000000000008001
{-# INLINABLE _p #-}

-- | Order of SECT193R1 curve.
_r :: Natural
_r = 0x1000000000000000000000000c7f34a778f443acc920eba49
{-# INLINABLE _r #-}

-- | Coordinate @X@ of SECT193R1 curve.
_x :: F2m
_x = 0x1f481bc5f0ff84a74ad6cdf6fdef4bf6179625372d8c0c5e1
{-# INLINABLE _x #-}

-- | Coordinate @Y@ of SECT193R1 curve.
_y :: F2m
_y = 0x25e399f2903712ccf3ea9e3a1ad17fb0b3201b6af7ce1b05
{-# INLINABLE _y #-}

-- | Generator of affine SECT193R1 curve.
gA :: PA
gA = A _x _y
{-# INLINABLE gA #-}

-- | Generator of projective SECT193R1 curve.
gP :: PP
gP = P _x _y 1
{-# INLINABLE gP #-}
