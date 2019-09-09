module Data.Curve.Weierstrass.SECP160K1
  ( module Data.Curve.Weierstrass
  -- * SECP160K1 curve
  , module Data.Curve.Weierstrass.SECP160K1
  ) where

import Protolude

import Data.Field.Galois
import GHC.Natural (Natural)

import Data.Curve.Weierstrass

-------------------------------------------------------------------------------
-- Types
-------------------------------------------------------------------------------

-- | SECP160K1 curve.
data SECP160K1

-- | Field of points of SECP160K1 curve.
type Fq = Prime Q
type Q = 0xfffffffffffffffffffffffffffffffeffffac73

-- | Field of coefficients of SECP160K1 curve.
type Fr = Prime R
type R = 0x100000000000000000001b8fa16dfab9aca16b6b3

-- SECP160K1 curve is a Weierstrass curve.
instance Curve 'Weierstrass c SECP160K1 Fq Fr => WCurve c SECP160K1 Fq Fr where
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

-- | Affine SECP160K1 curve point.
type PA = WAPoint SECP160K1 Fq Fr

-- Affine SECP160K1 curve is a Weierstrass affine curve.
instance WACurve SECP160K1 Fq Fr where
  gA_ = gA
  {-# INLINABLE gA_ #-}

-- | Jacobian SECP160K1 point.
type PJ = WJPoint SECP160K1 Fq Fr

-- Jacobian SECP160K1 curve is a Weierstrass Jacobian curve.
instance WJCurve SECP160K1 Fq Fr where
  gJ_ = gJ
  {-# INLINABLE gJ_ #-}

-- | Projective SECP160K1 point.
type PP = WPPoint SECP160K1 Fq Fr

-- Projective SECP160K1 curve is a Weierstrass projective curve.
instance WPCurve SECP160K1 Fq Fr where
  gP_ = gP
  {-# INLINABLE gP_ #-}

-------------------------------------------------------------------------------
-- Parameters
-------------------------------------------------------------------------------

-- | Coefficient @A@ of SECP160K1 curve.
_a :: Fq
_a = 0x0
{-# INLINABLE _a #-}

-- | Coefficient @B@ of SECP160K1 curve.
_b :: Fq
_b = 0x7
{-# INLINABLE _b #-}

-- | Cofactor of SECP160K1 curve.
_h :: Natural
_h = 0x1
{-# INLINABLE _h #-}

-- | Characteristic of SECP160K1 curve.
_q :: Natural
_q = 0xfffffffffffffffffffffffffffffffeffffac73
{-# INLINABLE _q #-}

-- | Order of SECP160K1 curve.
_r :: Natural
_r = 0x100000000000000000001b8fa16dfab9aca16b6b3
{-# INLINABLE _r #-}

-- | Coordinate @X@ of SECP160K1 curve.
_x :: Fq
_x = 0x3b4c382ce37aa192a4019e763036f4f5dd4d7ebb
{-# INLINABLE _x #-}

-- | Coordinate @Y@ of SECP160K1 curve.
_y :: Fq
_y = 0x938cf935318fdced6bc28286531733c3f03c4fee
{-# INLINABLE _y #-}

-- | Generator of affine SECP160K1 curve.
gA :: PA
gA = A _x _y
{-# INLINABLE gA #-}

-- | Generator of Jacobian SECP160K1 curve.
gJ :: PJ
gJ = J _x _y 1
{-# INLINABLE gJ #-}

-- | Generator of projective SECP160K1 curve.
gP :: PP
gP = P _x _y 1
{-# INLINABLE gP #-}
