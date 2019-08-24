module Data.Curve.Binary.SECT163R2
  ( module Data.Curve.Binary
  , module Data.Curve.Binary.SECT163R2
  , Point(..)
  ) where

import Protolude

import Data.Field.Galois

import Data.Curve.Binary

-------------------------------------------------------------------------------
-- Types
-------------------------------------------------------------------------------

-- | SECT163R2 curve.
data SECT163R2

-- | Field of points of SECT163R2 curve.
type F2m = Binary 0x800000000000000000000000000000000000000c9

-- | Field of coefficients of SECT163R2 curve.
type Fr = Prime 0x40000000000000000000292fe77e70c12a4234c33

-- | SECT163R2 curve is a binary curve.
instance Curve 'Binary c SECT163R2 F2m Fr => BCurve c SECT163R2 F2m Fr where
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

-- | Affine SECT163R2 curve point.
type PA = BAPoint SECT163R2 F2m Fr

-- | Affine SECT163R2 curve is a binary affine curve.
instance BACurve SECT163R2 F2m Fr where
  gA_ = gA
  {-# INLINABLE gA_ #-}

-- | Projective SECT163R2 point.
type PP = BPPoint SECT163R2 F2m Fr

-- | Projective SECT163R2 curve is a binary projective curve.
instance BPCurve SECT163R2 F2m Fr where
  gP_ = gP
  {-# INLINABLE gP_ #-}

-------------------------------------------------------------------------------
-- Parameters
-------------------------------------------------------------------------------

-- | Coefficient @A@ of SECT163R2 curve.
_a :: F2m
_a = 0x1
{-# INLINABLE _a #-}

-- | Coefficient @B@ of SECT163R2 curve.
_b :: F2m
_b = 0x20a601907b8c953ca1481eb10512f78744a3205fd
{-# INLINABLE _b #-}

-- | Cofactor of SECT163R2 curve.
_h :: Integer
_h = 0x2
{-# INLINABLE _h #-}

-- | Polynomial of SECT163R2 curve.
_p :: Integer
_p = 0x800000000000000000000000000000000000000c9
{-# INLINABLE _p #-}

-- | Order of SECT163R2 curve.
_r :: Integer
_r = 0x40000000000000000000292fe77e70c12a4234c33
{-# INLINABLE _r #-}

-- | Coordinate @X@ of SECT163R2 curve.
_x :: F2m
_x = 0x3f0eba16286a2d57ea0991168d4994637e8343e36
{-# INLINABLE _x #-}

-- | Coordinate @Y@ of SECT163R2 curve.
_y :: F2m
_y = 0xd51fbc6c71a0094fa2cdd545b11c5c0c797324f1
{-# INLINABLE _y #-}

-- | Generator of affine SECT163R2 curve.
gA :: PA
gA = A _x _y
{-# INLINABLE gA #-}

-- | Generator of projective SECT163R2 curve.
gP :: PP
gP = P _x _y 1
{-# INLINABLE gP #-}
