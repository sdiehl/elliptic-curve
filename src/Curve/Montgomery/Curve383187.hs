module Curve.Montgomery.Curve383187
  ( module Curve.Montgomery
  , module Curve.Montgomery.Curve383187
  , Point(..)
  ) where

import Protolude

import Data.Field.Galois

import Curve.Montgomery

-------------------------------------------------------------------------------
-- Types
-------------------------------------------------------------------------------

-- | Curve383187 curve.
data Curve383187

-- | Field of points of Curve383187 curve.
type Fq = Prime 0x7fffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff45

-- | Field of coefficients of Curve383187 curve.
type Fr = Prime 0x1000000000000000000000000000000000000000000000000e85a85287a1488acd41ae84b2b7030446f72088b00a0e21

-- | Curve383187 curve is a Montgomery curve.
instance Curve 'Montgomery c Curve383187 Fq Fr => MCurve c Curve383187 Fq Fr where
  a_ = const _a
  {-# INLINABLE a_ #-}
  b_ = const _b
  {-# INLINABLE b_ #-}
  h_ = const _h
  {-# INLINABLE h_ #-}
  q_ = const _q
  {-# INLINABLE q_ #-}
  r_ = const _r
  {-# INLINABLE r_ #-}
  x_ = const _x
  {-# INLINABLE x_ #-}
  y_ = const _y
  {-# INLINABLE y_ #-}

-- | Affine Curve383187 curve point.
type PA = MAPoint Curve383187 Fq Fr

-- | Affine Curve383187 curve is a Montgomery affine curve.
instance MACurve Curve383187 Fq Fr where
  gA_ = gA
  {-# INLINABLE gA_ #-}

-------------------------------------------------------------------------------
-- Parameters
-------------------------------------------------------------------------------

-- | Coefficient @A@ of Curve383187 curve.
_a :: Fq
_a = 0x38251
{-# INLINABLE _a #-}

-- | Coefficient @B@ of Curve383187 curve.
_b :: Fq
_b = 0x1
{-# INLINABLE _b #-}

-- | Cofactor of Curve383187 curve.
_h :: Integer
_h = 0x8
{-# INLINABLE _h #-}

-- | Characteristic of Curve383187 curve.
_q :: Integer
_q = 0x7fffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff45
{-# INLINABLE _q #-}

-- | Order of Curve383187 curve.
_r :: Integer
_r = 0x1000000000000000000000000000000000000000000000000e85a85287a1488acd41ae84b2b7030446f72088b00a0e21
{-# INLINABLE _r #-}

-- | Coordinate @X@ of Curve383187 curve.
_x :: Fq
_x = 0x5
{-# INLINABLE _x #-}

-- | Coordinate @Y@ of Curve383187 curve.
_y :: Fq
_y = 0x1eebe07dc1871896732b12d5504a32370471965c7a11f2c89865f855ab3cbd7c224e3620c31af3370788457dd5ce46df
{-# INLINABLE _y #-}

-- | Generator of affine Curve383187 curve.
gA :: PA
gA = A _x _y
{-# INLINABLE gA #-}
