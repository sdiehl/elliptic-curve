module Curve.Binary.SECT571R1
  ( module Curve.Binary
  , module Curve.Binary.SECT571R1
  , Point(..)
  ) where

import Protolude

import Data.Field.Galois

import Curve.Binary

-------------------------------------------------------------------------------
-- Types
-------------------------------------------------------------------------------

-- | SECT571R1 curve.
data SECT571R1

-- | Field of points of SECT571R1 curve.
type F2m = Binary 0x80000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000425

-- | Field of coefficients of SECT571R1 curve.
type Fr = Prime 0x3ffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffe661ce18ff55987308059b186823851ec7dd9ca1161de93d5174d66e8382e9bb2fe84e47

-- | SECT571R1 curve is a binary curve.
instance Curve 'Binary c SECT571R1 F2m Fr => BCurve c SECT571R1 F2m Fr where
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

-- | Affine SECT571R1 curve point.
type PA = BAPoint SECT571R1 F2m Fr

-- | Affine SECT571R1 curve is a binary affine curve.
instance BACurve SECT571R1 F2m Fr where
  gA_ = gA
  {-# INLINABLE gA_ #-}

-- | Projective SECT571R1 point.
type PP = BPPoint SECT571R1 F2m Fr

-- | Projective SECT571R1 curve is a binary projective curve.
instance BPCurve SECT571R1 F2m Fr where
  gP_ = gP
  {-# INLINABLE gP_ #-}

-------------------------------------------------------------------------------
-- Parameters
-------------------------------------------------------------------------------

-- | Coefficient @A@ of SECT571R1 curve.
_a :: F2m
_a = 0x1
{-# INLINABLE _a #-}

-- | Coefficient @B@ of SECT571R1 curve.
_b :: F2m
_b = 0x2f40e7e2221f295de297117b7f3d62f5c6a97ffcb8ceff1cd6ba8ce4a9a18ad84ffabbd8efa59332be7ad6756a66e294afd185a78ff12aa520e4de739baca0c7ffeff7f2955727a
{-# INLINABLE _b #-}

-- | Cofactor of SECT571R1 curve.
_h :: Integer
_h = 0x2
{-# INLINABLE _h #-}

-- | Polynomial of SECT571R1 curve.
_p :: Integer
_p = 0x80000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000425
{-# INLINABLE _p #-}

-- | Order of SECT571R1 curve.
_r :: Integer
_r = 0x3ffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffe661ce18ff55987308059b186823851ec7dd9ca1161de93d5174d66e8382e9bb2fe84e47
{-# INLINABLE _r #-}

-- | Coordinate @X@ of SECT571R1 curve.
_x :: F2m
_x = 0x303001d34b856296c16c0d40d3cd7750a93d1d2955fa80aa5f40fc8db7b2abdbde53950f4c0d293cdd711a35b67fb1499ae60038614f1394abfa3b4c850d927e1e7769c8eec2d19
{-# INLINABLE _x #-}

-- | Coordinate @Y@ of SECT571R1 curve.
_y :: F2m
_y = 0x37bf27342da639b6dccfffeb73d69d78c6c27a6009cbbca1980f8533921e8a684423e43bab08a576291af8f461bb2a8b3531d2f0485c19b16e2f1516e23dd3c1a4827af1b8ac15b
{-# INLINABLE _y #-}

-- | Generator of affine SECT571R1 curve.
gA :: PA
gA = A _x _y
{-# INLINABLE gA #-}

-- | Generator of projective SECT571R1 curve.
gP :: PP
gP = P _x _y 1
{-# INLINABLE gP #-}
