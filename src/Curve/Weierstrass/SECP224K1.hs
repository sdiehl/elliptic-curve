module Curve.Weierstrass.SECP224K1
  ( module Curve.Weierstrass
  , module Curve.Weierstrass.SECP224K1
  ) where

import Protolude

import PrimeField

import Curve.Weierstrass

-------------------------------------------------------------------------------
-- Types
-------------------------------------------------------------------------------

-- | SECP224K1 curve.
data SECP224K1

-- | Field of points of SECP224K1 curve.
type Fq = PrimeField 0xfffffffffffffffffffffffffffffffffffffffffffffffeffffe56d

-- | Field of coefficients of SECP224K1 curve.
type Fr = PrimeField 0x10000000000000000000000000001dce8d2ec6184caf0a971769fb1f7

-- | SECP224K1 curve is a Weierstrass curve.
instance Curve 'Weierstrass c SECP224K1 Fq Fr => WCurve c SECP224K1 Fq Fr where
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

-- | Affine SECP224K1 curve point.
type PA = WAPoint SECP224K1 Fq Fr

-- | Affine SECP224K1 curve is a Weierstrass affine curve.
instance WACurve SECP224K1 Fq Fr where
  gA_ = gA
  {-# INLINABLE gA_ #-}

-- | Jacobian SECP224K1 point.
type PJ = WJPoint SECP224K1 Fq Fr

-- | Jacobian SECP224K1 curve is a Weierstrass Jacobian curve.
instance WJCurve SECP224K1 Fq Fr where
  gJ_ = gJ
  {-# INLINABLE gJ_ #-}

-- | Projective SECP224K1 point.
type PP = WPPoint SECP224K1 Fq Fr

-- | Projective SECP224K1 curve is a Weierstrass projective curve.
instance WPCurve SECP224K1 Fq Fr where
  gP_ = gP
  {-# INLINABLE gP_ #-}

-------------------------------------------------------------------------------
-- Parameters
-------------------------------------------------------------------------------

-- | Coefficient @A@ of SECP224K1 curve.
_a :: Fq
_a = 0x0
{-# INLINABLE _a #-}

-- | Coefficient @B@ of SECP224K1 curve.
_b :: Fq
_b = 0x5
{-# INLINABLE _b #-}

-- | Cofactor of SECP224K1 curve.
_h :: Integer
_h = 0x1
{-# INLINABLE _h #-}

-- | Characteristic of SECP224K1 curve.
_q :: Integer
_q = 0xfffffffffffffffffffffffffffffffffffffffffffffffeffffe56d
{-# INLINABLE _q #-}

-- | Order of SECP224K1 curve.
_r :: Integer
_r = 0x10000000000000000000000000001dce8d2ec6184caf0a971769fb1f7
{-# INLINABLE _r #-}

-- | Coordinate @X@ of SECP224K1 curve.
_x :: Fq
_x = 0xa1455b334df099df30fc28a169a467e9e47075a90f7e650eb6b7a45c
{-# INLINABLE _x #-}

-- | Coordinate @Y@ of SECP224K1 curve.
_y :: Fq
_y = 0x7e089fed7fba344282cafbd6f7e319f7c0b0bd59e2ca4bdb556d61a5
{-# INLINABLE _y #-}

-- | Generator of affine SECP224K1 curve.
gA :: PA
gA = A _x _y
{-# INLINABLE gA #-}

-- | Generator of Jacobian SECP224K1 curve.
gJ :: PJ
gJ = J _x _y 1
{-# INLINABLE gJ #-}

-- | Generator of projective SECP224K1 curve.
gP :: PP
gP = P _x _y 1
{-# INLINABLE gP #-}
