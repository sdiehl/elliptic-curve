module Data.Curve.Binary.SECT113R2
  ( module Data.Curve.Binary
  , module Data.Curve.Binary.SECT113R2
  , Point(..)
  ) where

import Protolude

import Data.Field.Galois

import Data.Curve.Binary

-------------------------------------------------------------------------------
-- Types
-------------------------------------------------------------------------------

-- | SECT113R2 curve.
data SECT113R2

-- | Field of points of SECT113R2 curve.
type F2m = Binary 0x20000000000000000000000000201

-- | Field of coefficients of SECT113R2 curve.
type Fr = Prime 0x10000000000000108789b2496af93

-- | SECT113R2 curve is a binary curve.
instance Curve 'Binary c SECT113R2 F2m Fr => BCurve c SECT113R2 F2m Fr where
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

-- | Affine SECT113R2 curve point.
type PA = BAPoint SECT113R2 F2m Fr

-- | Affine SECT113R2 curve is a binary affine curve.
instance BACurve SECT113R2 F2m Fr where
  gA_ = gA
  {-# INLINABLE gA_ #-}

-- | Projective SECT113R2 point.
type PP = BPPoint SECT113R2 F2m Fr

-- | Projective SECT113R2 curve is a binary projective curve.
instance BPCurve SECT113R2 F2m Fr where
  gP_ = gP
  {-# INLINABLE gP_ #-}

-------------------------------------------------------------------------------
-- Parameters
-------------------------------------------------------------------------------

-- | Coefficient @A@ of SECT113R2 curve.
_a :: F2m
_a = 0x689918dbec7e5a0dd6dfc0aa55c7
{-# INLINABLE _a #-}

-- | Coefficient @B@ of SECT113R2 curve.
_b :: F2m
_b = 0x95e9a9ec9b297bd4bf36e059184f
{-# INLINABLE _b #-}

-- | Cofactor of SECT113R2 curve.
_h :: Integer
_h = 0x2
{-# INLINABLE _h #-}

-- | Polynomial of SECT113R2 curve.
_p :: Integer
_p = 0x20000000000000000000000000201
{-# INLINABLE _p #-}

-- | Order of SECT113R2 curve.
_r :: Integer
_r = 0x10000000000000108789b2496af93
{-# INLINABLE _r #-}

-- | Coordinate @X@ of SECT113R2 curve.
_x :: F2m
_x = 0x1a57a6a7b26ca5ef52fcdb8164797
{-# INLINABLE _x #-}

-- | Coordinate @Y@ of SECT113R2 curve.
_y :: F2m
_y = 0xb3adc94ed1fe674c06e695baba1d
{-# INLINABLE _y #-}

-- | Generator of affine SECT113R2 curve.
gA :: PA
gA = A _x _y
{-# INLINABLE gA #-}

-- | Generator of projective SECT113R2 curve.
gP :: PP
gP = P _x _y 1
{-# INLINABLE gP #-}
