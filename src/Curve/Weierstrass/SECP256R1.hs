module Curve.Weierstrass.SECP256R1
  ( module Curve.Weierstrass
  , module Curve.Weierstrass.SECP256R1
  , Point(..)
  ) where

import Protolude

import PrimeField

import Curve.Weierstrass

-------------------------------------------------------------------------------
-- Types
-------------------------------------------------------------------------------

-- | SECP256R1 curve.
data SECP256R1

-- | Field of points of SECP256R1 curve.
type Fq = PrimeField 0xffffffff00000001000000000000000000000000ffffffffffffffffffffffff

-- | Field of coefficients of SECP256R1 curve.
type Fr = PrimeField 0xffffffff00000000ffffffffffffffffbce6faada7179e84f3b9cac2fc632551

-- | SECP256R1 curve is a Weierstrass curve.
instance Curve 'Weierstrass c SECP256R1 Fq Fr => WCurve c SECP256R1 Fq Fr where
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

-- | Affine SECP256R1 curve point.
type PA = WAPoint SECP256R1 Fq Fr

-- | Affine SECP256R1 curve is a Weierstrass affine curve.
instance WACurve SECP256R1 Fq Fr where
  gA_ = gA
  {-# INLINABLE gA_ #-}

-- | Jacobian SECP256R1 point.
type PJ = WJPoint SECP256R1 Fq Fr

-- | Jacobian SECP256R1 curve is a Weierstrass Jacobian curve.
instance WJCurve SECP256R1 Fq Fr where
  gJ_ = gJ
  {-# INLINABLE gJ_ #-}

-- | Projective SECP256R1 point.
type PP = WPPoint SECP256R1 Fq Fr

-- | Projective SECP256R1 curve is a Weierstrass projective curve.
instance WPCurve SECP256R1 Fq Fr where
  gP_ = gP
  {-# INLINABLE gP_ #-}

-------------------------------------------------------------------------------
-- Parameters
-------------------------------------------------------------------------------

-- | Coefficient @A@ of SECP256R1 curve.
_a :: Fq
_a = 0xffffffff00000001000000000000000000000000fffffffffffffffffffffffc
{-# INLINABLE _a #-}

-- | Coefficient @B@ of SECP256R1 curve.
_b :: Fq
_b = 0x5ac635d8aa3a93e7b3ebbd55769886bc651d06b0cc53b0f63bce3c3e27d2604b
{-# INLINABLE _b #-}

-- | Cofactor of SECP256R1 curve.
_h :: Integer
_h = 0x1
{-# INLINABLE _h #-}

-- | Characteristic of SECP256R1 curve.
_q :: Integer
_q = 0xffffffff00000001000000000000000000000000ffffffffffffffffffffffff
{-# INLINABLE _q #-}

-- | Order of SECP256R1 curve.
_r :: Integer
_r = 0xffffffff00000000ffffffffffffffffbce6faada7179e84f3b9cac2fc632551
{-# INLINABLE _r #-}

-- | Coordinate @X@ of SECP256R1 curve.
_x :: Fq
_x = 0x6b17d1f2e12c4247f8bce6e563a440f277037d812deb33a0f4a13945d898c296
{-# INLINABLE _x #-}

-- | Coordinate @Y@ of SECP256R1 curve.
_y :: Fq
_y = 0x4fe342e2fe1a7f9b8ee7eb4a7c0f9e162bce33576b315ececbb6406837bf51f5
{-# INLINABLE _y #-}

-- | Generator of affine SECP256R1 curve.
gA :: PA
gA = A _x _y
{-# INLINABLE gA #-}

-- | Generator of Jacobian SECP256R1 curve.
gJ :: PJ
gJ = J _x _y 1
{-# INLINABLE gJ #-}

-- | Generator of projective SECP256R1 curve.
gP :: PP
gP = P _x _y 1
{-# INLINABLE gP #-}
