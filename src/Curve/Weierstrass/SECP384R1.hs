module Curve.Weierstrass.SECP384R1
  ( module Curve.Weierstrass
  , module Curve.Weierstrass.SECP384R1
  ) where

import Protolude

import PrimeField

import Curve.Weierstrass

-------------------------------------------------------------------------------
-- Types
-------------------------------------------------------------------------------

-- | SECP384R1 curve.
data SECP384R1

-- | Field of points of SECP384R1 curve.
type Fq = PrimeField 0xfffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffeffffffff0000000000000000ffffffff

-- | Field of coefficients of SECP384R1 curve.
type Fr = PrimeField 0xffffffffffffffffffffffffffffffffffffffffffffffffc7634d81f4372ddf581a0db248b0a77aecec196accc52973

-- | SECP384R1 curve is a Weierstrass curve.
instance Curve 'Weierstrass c SECP384R1 Fq Fr => WCurve c SECP384R1 Fq Fr where
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

-- | Affine SECP384R1 curve point.
type PA = WAPoint SECP384R1 Fq Fr

-- | Affine SECP384R1 curve is a Weierstrass affine curve.
instance WACurve SECP384R1 Fq Fr where
  gA_ = gA
  {-# INLINABLE gA_ #-}

-- | Jacobian SECP384R1 point.
type PJ = WJPoint SECP384R1 Fq Fr

-- | Jacobian SECP384R1 curve is a Weierstrass Jacobian curve.
instance WJCurve SECP384R1 Fq Fr where
  gJ_ = gJ
  {-# INLINABLE gJ_ #-}

-- | Projective SECP384R1 point.
type PP = WPPoint SECP384R1 Fq Fr

-- | Projective SECP384R1 curve is a Weierstrass projective curve.
instance WPCurve SECP384R1 Fq Fr where
  gP_ = gP
  {-# INLINABLE gP_ #-}

-------------------------------------------------------------------------------
-- Parameters
-------------------------------------------------------------------------------

-- | Coefficient @A@ of SECP384R1 curve.
_a :: Fq
_a = 0xfffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffeffffffff0000000000000000fffffffc
{-# INLINABLE _a #-}

-- | Coefficient @B@ of SECP384R1 curve.
_b :: Fq
_b = 0xb3312fa7e23ee7e4988e056be3f82d19181d9c6efe8141120314088f5013875ac656398d8a2ed19d2a85c8edd3ec2aef
{-# INLINABLE _b #-}

-- | Cofactor of SECP384R1 curve.
_h :: Integer
_h = 0x1
{-# INLINABLE _h #-}

-- | Characteristic of SECP384R1 curve.
_q :: Integer
_q = 0xfffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffeffffffff0000000000000000ffffffff
{-# INLINABLE _q #-}

-- | Order of SECP384R1 curve.
_r :: Integer
_r = 0xffffffffffffffffffffffffffffffffffffffffffffffffc7634d81f4372ddf581a0db248b0a77aecec196accc52973
{-# INLINABLE _r #-}

-- | Coordinate @X@ of SECP384R1 curve.
_x :: Fq
_x = 0xaa87ca22be8b05378eb1c71ef320ad746e1d3b628ba79b9859f741e082542a385502f25dbf55296c3a545e3872760ab7
{-# INLINABLE _x #-}

-- | Coordinate @Y@ of SECP384R1 curve.
_y :: Fq
_y = 0x3617de4a96262c6f5d9e98bf9292dc29f8f41dbd289a147ce9da3113b5f0b8c00a60b1ce1d7e819d7a431d7c90ea0e5f
{-# INLINABLE _y #-}

-- | Generator of affine SECP384R1 curve.
gA :: PA
gA = A _x _y
{-# INLINABLE gA #-}

-- | Generator of Jacobian SECP384R1 curve.
gJ :: PJ
gJ = J _x _y 1
{-# INLINABLE gJ #-}

-- | Generator of projective SECP384R1 curve.
gP :: PP
gP = P _x _y 1
{-# INLINABLE gP #-}
