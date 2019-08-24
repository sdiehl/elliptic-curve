module Curve.Montgomery.M511
  ( module Curve.Montgomery
  , module Curve.Montgomery.M511
  , Point(..)
  ) where

import Protolude

import Data.Field.Galois

import Curve.Montgomery

-------------------------------------------------------------------------------
-- Types
-------------------------------------------------------------------------------

-- | M511 curve.
data M511

-- | Field of points of M511 curve.
type Fq = Prime 0x7fffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff45

-- | Field of coefficients of M511 curve.
type Fr = Prime 0x100000000000000000000000000000000000000000000000000000000000000017b5feff30c7f5677ab2aeebd13779a2ac125042a6aa10bfa54c15bab76baf1b

-- | M511 curve is a Montgomery curve.
instance Curve 'Montgomery c M511 Fq Fr => MCurve c M511 Fq Fr where
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

-- | Affine M511 curve point.
type PA = MAPoint M511 Fq Fr

-- | Affine M511 curve is a Montgomery affine curve.
instance MACurve M511 Fq Fr where
  gA_ = gA
  {-# INLINABLE gA_ #-}

-------------------------------------------------------------------------------
-- Parameters
-------------------------------------------------------------------------------

-- | Coefficient @A@ of M511 curve.
_a :: Fq
_a = 0x81806
{-# INLINABLE _a #-}

-- | Coefficient @B@ of M511 curve.
_b :: Fq
_b = 0x1
{-# INLINABLE _b #-}

-- | Cofactor of M511 curve.
_h :: Integer
_h = 0x8
{-# INLINABLE _h #-}

-- | Characteristic of M511 curve.
_q :: Integer
_q = 0x7fffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff45
{-# INLINABLE _q #-}

-- | Order of M511 curve.
_r :: Integer
_r = 0x100000000000000000000000000000000000000000000000000000000000000017b5feff30c7f5677ab2aeebd13779a2ac125042a6aa10bfa54c15bab76baf1b
{-# INLINABLE _r #-}

-- | Coordinate @X@ of M511 curve.
_x :: Fq
_x = 0x5
{-# INLINABLE _x #-}

-- | Coordinate @Y@ of M511 curve.
_y :: Fq
_y = 0x2fbdc0ad8530803d28fdbad354bb488d32399ac1cf8f6e01ee3f96389b90c809422b9429e8a43dbf49308ac4455940abe9f1dbca542093a895e30a64af056fa5
{-# INLINABLE _y #-}

-- | Generator of affine M511 curve.
gA :: PA
gA = A _x _y
{-# INLINABLE gA #-}
