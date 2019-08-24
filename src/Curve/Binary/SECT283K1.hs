module Curve.Binary.SECT283K1
  ( module Curve.Binary
  , module Curve.Binary.SECT283K1
  , Point(..)
  ) where

import Protolude

import Data.Field.Galois

import Curve.Binary

-------------------------------------------------------------------------------
-- Types
-------------------------------------------------------------------------------

-- | SECT283K1 curve.
data SECT283K1

-- | Field of points of SECT283K1 curve.
type F2m = Binary 0x800000000000000000000000000000000000000000000000000000000000000000010a1

-- | Field of coefficients of SECT283K1 curve.
type Fr = Prime 0x1ffffffffffffffffffffffffffffffffffe9ae2ed07577265dff7f94451e061e163c61

-- | SECT283K1 curve is a binary curve.
instance Curve 'Binary c SECT283K1 F2m Fr => BCurve c SECT283K1 F2m Fr where
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

-- | Affine SECT283K1 curve point.
type PA = BAPoint SECT283K1 F2m Fr

-- | Affine SECT283K1 curve is a binary affine curve.
instance BACurve SECT283K1 F2m Fr where
  gA_ = gA
  {-# INLINABLE gA_ #-}

-- | Projective SECT283K1 point.
type PP = BPPoint SECT283K1 F2m Fr

-- | Projective SECT283K1 curve is a binary projective curve.
instance BPCurve SECT283K1 F2m Fr where
  gP_ = gP
  {-# INLINABLE gP_ #-}

-------------------------------------------------------------------------------
-- Parameters
-------------------------------------------------------------------------------

-- | Coefficient @A@ of SECT283K1 curve.
_a :: F2m
_a = 0x0
{-# INLINABLE _a #-}

-- | Coefficient @B@ of SECT283K1 curve.
_b :: F2m
_b = 0x1
{-# INLINABLE _b #-}

-- | Cofactor of SECT283K1 curve.
_h :: Integer
_h = 0x4
{-# INLINABLE _h #-}

-- | Polynomial of SECT283K1 curve.
_p :: Integer
_p = 0x800000000000000000000000000000000000000000000000000000000000000000010a1
{-# INLINABLE _p #-}

-- | Order of SECT283K1 curve.
_r :: Integer
_r = 0x1ffffffffffffffffffffffffffffffffffe9ae2ed07577265dff7f94451e061e163c61
{-# INLINABLE _r #-}

-- | Coordinate @X@ of SECT283K1 curve.
_x :: F2m
_x = 0x503213f78ca44883f1a3b8162f188e553cd265f23c1567a16876913b0c2ac2458492836
{-# INLINABLE _x #-}

-- | Coordinate @Y@ of SECT283K1 curve.
_y :: F2m
_y = 0x1ccda380f1c9e318d90f95d07e5426fe87e45c0e8184698e45962364e34116177dd2259
{-# INLINABLE _y #-}

-- | Generator of affine SECT283K1 curve.
gA :: PA
gA = A _x _y
{-# INLINABLE gA #-}

-- | Generator of projective SECT283K1 curve.
gP :: PP
gP = P _x _y 1
{-# INLINABLE gP #-}
