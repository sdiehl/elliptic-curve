module Data.Curve.Binary.SECT163K1
  ( module Data.Curve.Binary
  -- * SECT163K1 curve
  , module Data.Curve.Binary.SECT163K1
  ) where

import Protolude

import Data.Field.Galois
import GHC.Natural (Natural)

import Data.Curve.Binary
import Data.Curve.Binary.Base (BCurve(..), BACurve(..), BPCurve(..))

-------------------------------------------------------------------------------
-- Types
-------------------------------------------------------------------------------

-- | SECT163K1 curve.
data SECT163K1

-- | Field of points of SECT163K1 curve.
type F2m = Binary 0x800000000000000000000000000000000000000c9

-- | Field of coefficients of SECT163K1 curve.
type Fr = Prime 0x4000000000000000000020108a2e0cc0d99f8a5ef

-- SECT163K1 curve is a binary curve.
instance Curve 'Binary c SECT163K1 F2m Fr => BCurve c SECT163K1 F2m Fr where
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

-- | Affine SECT163K1 curve point.
type PA = BAPoint SECT163K1 F2m Fr

-- Affine SECT163K1 curve is a binary affine curve.
instance BACurve SECT163K1 F2m Fr where
  gA_ = gA
  {-# INLINABLE gA_ #-}

-- | Projective SECT163K1 point.
type PP = BPPoint SECT163K1 F2m Fr

-- Projective SECT163K1 curve is a binary projective curve.
instance BPCurve SECT163K1 F2m Fr where
  gP_ = gP
  {-# INLINABLE gP_ #-}

-------------------------------------------------------------------------------
-- Parameters
-------------------------------------------------------------------------------

-- | Coefficient @A@ of SECT163K1 curve.
_a :: F2m
_a = 0x1
{-# INLINABLE _a #-}

-- | Coefficient @B@ of SECT163K1 curve.
_b :: F2m
_b = 0x1
{-# INLINABLE _b #-}

-- | Cofactor of SECT163K1 curve.
_h :: Natural
_h = 0x2
{-# INLINABLE _h #-}

-- | Polynomial of SECT163K1 curve.
_p :: Natural
_p = 0x800000000000000000000000000000000000000c9
{-# INLINABLE _p #-}

-- | Order of SECT163K1 curve.
_r :: Natural
_r = 0x4000000000000000000020108a2e0cc0d99f8a5ef
{-# INLINABLE _r #-}

-- | Coordinate @X@ of SECT163K1 curve.
_x :: F2m
_x = 0x2fe13c0537bbc11acaa07d793de4e6d5e5c94eee8
{-# INLINABLE _x #-}

-- | Coordinate @Y@ of SECT163K1 curve.
_y :: F2m
_y = 0x289070fb05d38ff58321f2e800536d538ccdaa3d9
{-# INLINABLE _y #-}

-- | Generator of affine SECT163K1 curve.
gA :: PA
gA = A _x _y
{-# INLINABLE gA #-}

-- | Generator of projective SECT163K1 curve.
gP :: PP
gP = P _x _y 1
{-# INLINABLE gP #-}
