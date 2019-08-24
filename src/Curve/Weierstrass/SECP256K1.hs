module Curve.Weierstrass.SECP256K1
  ( module Curve.Weierstrass
  , module Curve.Weierstrass.SECP256K1
  , Point(..)
  ) where

import Protolude

import Data.Field.Galois

import Curve.Weierstrass

-------------------------------------------------------------------------------
-- Types
-------------------------------------------------------------------------------

-- | SECP256K1 curve.
data SECP256K1

-- | Field of points of SECP256K1 curve.
type Fq = Prime 0xfffffffffffffffffffffffffffffffffffffffffffffffffffffffefffffc2f

-- | Field of coefficients of SECP256K1 curve.
type Fr = Prime 0xfffffffffffffffffffffffffffffffebaaedce6af48a03bbfd25e8cd0364141

-- | SECP256K1 curve is a Weierstrass curve.
instance Curve 'Weierstrass c SECP256K1 Fq Fr => WCurve c SECP256K1 Fq Fr where
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

-- | Affine SECP256K1 curve point.
type PA = WAPoint SECP256K1 Fq Fr

-- | Affine SECP256K1 curve is a Weierstrass affine curve.
instance WACurve SECP256K1 Fq Fr where
  gA_ = gA
  {-# INLINABLE gA_ #-}

-- | Jacobian SECP256K1 point.
type PJ = WJPoint SECP256K1 Fq Fr

-- | Jacobian SECP256K1 curve is a Weierstrass Jacobian curve.
instance WJCurve SECP256K1 Fq Fr where
  gJ_ = gJ
  {-# INLINABLE gJ_ #-}

-- | Projective SECP256K1 point.
type PP = WPPoint SECP256K1 Fq Fr

-- | Projective SECP256K1 curve is a Weierstrass projective curve.
instance WPCurve SECP256K1 Fq Fr where
  gP_ = gP
  {-# INLINABLE gP_ #-}

-------------------------------------------------------------------------------
-- Parameters
-------------------------------------------------------------------------------

-- | Coefficient @A@ of SECP256K1 curve.
_a :: Fq
_a = 0x0
{-# INLINABLE _a #-}

-- | Coefficient @B@ of SECP256K1 curve.
_b :: Fq
_b = 0x7
{-# INLINABLE _b #-}

-- | Cofactor of SECP256K1 curve.
_h :: Integer
_h = 0x1
{-# INLINABLE _h #-}

-- | Characteristic of SECP256K1 curve.
_q :: Integer
_q = 0xfffffffffffffffffffffffffffffffffffffffffffffffffffffffefffffc2f
{-# INLINABLE _q #-}

-- | Order of SECP256K1 curve.
_r :: Integer
_r = 0xfffffffffffffffffffffffffffffffebaaedce6af48a03bbfd25e8cd0364141
{-# INLINABLE _r #-}

-- | Coordinate @X@ of SECP256K1 curve.
_x :: Fq
_x = 0x79be667ef9dcbbac55a06295ce870b07029bfcdb2dce28d959f2815b16f81798
{-# INLINABLE _x #-}

-- | Coordinate @Y@ of SECP256K1 curve.
_y :: Fq
_y = 0x483ada7726a3c4655da4fbfc0e1108a8fd17b448a68554199c47d08ffb10d4b8
{-# INLINABLE _y #-}

-- | Generator of affine SECP256K1 curve.
gA :: PA
gA = A _x _y
{-# INLINABLE gA #-}

-- | Generator of Jacobian SECP256K1 curve.
gJ :: PJ
gJ = J _x _y 1
{-# INLINABLE gJ #-}

-- | Generator of projective SECP256K1 curve.
gP :: PP
gP = P _x _y 1
{-# INLINABLE gP #-}
