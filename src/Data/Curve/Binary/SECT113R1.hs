module Data.Curve.Binary.SECT113R1
  ( module Data.Curve.Binary
  -- * SECT113R1 curve
  , module Data.Curve.Binary.SECT113R1
  ) where

import Protolude

import Data.Field.Galois
import GHC.Natural (Natural)

import Data.Curve.Binary
import Data.Curve.Binary.Base (BCurve(..), BACurve(..), BPCurve(..))

-------------------------------------------------------------------------------
-- Types
-------------------------------------------------------------------------------

-- | SECT113R1 curve.
data SECT113R1

-- | Field of points of SECT113R1 curve.
type F2m = Binary M
type M = 0x20000000000000000000000000201

-- | Field of coefficients of SECT113R1 curve.
type Fr = Prime R
type R = 0x100000000000000d9ccec8a39e56f

-- SECT113R1 curve is a binary curve.
instance Curve 'Binary c SECT113R1 F2m Fr => BCurve c SECT113R1 F2m Fr where
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

-- | Affine SECT113R1 curve point.
type PA = BAPoint SECT113R1 F2m Fr

-- Affine SECT113R1 curve is a binary affine curve.
instance BACurve SECT113R1 F2m Fr where
  gA_ = gA
  {-# INLINABLE gA_ #-}

-- | Projective SECT113R1 point.
type PP = BPPoint SECT113R1 F2m Fr

-- Projective SECT113R1 curve is a binary projective curve.
instance BPCurve SECT113R1 F2m Fr where
  gP_ = gP
  {-# INLINABLE gP_ #-}

-------------------------------------------------------------------------------
-- Parameters
-------------------------------------------------------------------------------

-- | Coefficient @A@ of SECT113R1 curve.
_a :: F2m
_a = 0x3088250ca6e7c7fe649ce85820f7
{-# INLINABLE _a #-}

-- | Coefficient @B@ of SECT113R1 curve.
_b :: F2m
_b = 0xe8bee4d3e2260744188be0e9c723
{-# INLINABLE _b #-}

-- | Cofactor of SECT113R1 curve.
_h :: Natural
_h = 0x2
{-# INLINABLE _h #-}

-- | Polynomial of SECT113R1 curve.
_p :: Natural
_p = 0x20000000000000000000000000201
{-# INLINABLE _p #-}

-- | Order of SECT113R1 curve.
_r :: Natural
_r = 0x100000000000000d9ccec8a39e56f
{-# INLINABLE _r #-}

-- | Coordinate @X@ of SECT113R1 curve.
_x :: F2m
_x = 0x9d73616f35f4ab1407d73562c10f
{-# INLINABLE _x #-}

-- | Coordinate @Y@ of SECT113R1 curve.
_y :: F2m
_y = 0xa52830277958ee84d1315ed31886
{-# INLINABLE _y #-}

-- | Generator of affine SECT113R1 curve.
gA :: PA
gA = A _x _y
{-# INLINABLE gA #-}

-- | Generator of projective SECT113R1 curve.
gP :: PP
gP = P _x _y 1
{-# INLINABLE gP #-}
