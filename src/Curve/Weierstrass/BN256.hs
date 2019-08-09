module Curve.Weierstrass.BN256
  ( module Curve.Weierstrass
  , module Curve.Weierstrass.BN256
  ) where

import Protolude

import PrimeField

import Curve.Weierstrass

-------------------------------------------------------------------------------
-- Types
-------------------------------------------------------------------------------

-- | BN256 curve.
data BN256

-- | Field of points of BN256 curve.
type Fq = PrimeField 0xfffffffffffcf0cd46e5f25eee71a49f0cdc65fb12980a82d3292ddbaed33013

-- | Field of coefficients of BN256 curve.
type Fr = PrimeField 0xfffffffffffcf0cd46e5f25eee71a49e0cdc65fb1299921af62d536cd10b500d

-- | BN256 curve is a Weierstrass curve.
instance Curve 'Weierstrass c BN256 Fq Fr => WCurve c BN256 Fq Fr where
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

-- | Affine BN256 curve point.
type PA = WAPoint BN256 Fq Fr

-- | Affine BN256 curve is a Weierstrass affine curve.
instance WACurve BN256 Fq Fr where
  gA_ = gA
  {-# INLINABLE gA_ #-}

-- | Jacobian BN256 point.
type PJ = WJPoint BN256 Fq Fr

-- | Jacobian BN256 curve is a Weierstrass Jacobian curve.
instance WJCurve BN256 Fq Fr where
  gJ_ = gJ
  {-# INLINABLE gJ_ #-}

-- | Projective BN256 point.
type PP = WPPoint BN256 Fq Fr

-- | Projective BN256 curve is a Weierstrass projective curve.
instance WPCurve BN256 Fq Fr where
  gP_ = gP
  {-# INLINABLE gP_ #-}

-------------------------------------------------------------------------------
-- Parameters
-------------------------------------------------------------------------------

-- | Coefficient @A@ of BN256 curve.
_a :: Fq
_a = 0x0
{-# INLINABLE _a #-}

-- | Coefficient @B@ of BN256 curve.
_b :: Fq
_b = 0x3
{-# INLINABLE _b #-}

-- | Cofactor of BN256 curve.
_h :: Integer
_h = 0x1
{-# INLINABLE _h #-}

-- | Characteristic of BN256 curve.
_q :: Integer
_q = 0xfffffffffffcf0cd46e5f25eee71a49f0cdc65fb12980a82d3292ddbaed33013
{-# INLINABLE _q #-}

-- | Order of BN256 curve.
_r :: Integer
_r = 0xfffffffffffcf0cd46e5f25eee71a49e0cdc65fb1299921af62d536cd10b500d
{-# INLINABLE _r #-}

-- | Coordinate @X@ of BN256 curve.
_x :: Fq
_x = 0x1
{-# INLINABLE _x #-}

-- | Coordinate @Y@ of BN256 curve.
_y :: Fq
_y = 0x2
{-# INLINABLE _y #-}

-- | Generator of affine BN256 curve.
gA :: PA
gA = A _x _y
{-# INLINABLE gA #-}

-- | Generator of Jacobian BN256 curve.
gJ :: PJ
gJ = J _x _y 1
{-# INLINABLE gJ #-}

-- | Generator of projective BN256 curve.
gP :: PP
gP = P _x _y 1
{-# INLINABLE gP #-}
