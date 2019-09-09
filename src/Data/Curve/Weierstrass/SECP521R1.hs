module Data.Curve.Weierstrass.SECP521R1
  ( module Data.Curve.Weierstrass
  -- * SECP521R1 curve
  , module Data.Curve.Weierstrass.SECP521R1
  ) where

import Protolude

import Data.Field.Galois
import GHC.Natural (Natural)

import Data.Curve.Weierstrass

-------------------------------------------------------------------------------
-- Types
-------------------------------------------------------------------------------

-- | SECP521R1 curve.
data SECP521R1

-- | Field of points of SECP521R1 curve.
type Fq = Prime Q
type Q = 0x1ffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff

-- | Field of coefficients of SECP521R1 curve.
type Fr = Prime R
type R = 0x1fffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffa51868783bf2f966b7fcc0148f709a5d03bb5c9b8899c47aebb6fb71e91386409

-- SECP521R1 curve is a Weierstrass curve.
instance Curve 'Weierstrass c SECP521R1 Fq Fr => WCurve c SECP521R1 Fq Fr where
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

-- | Affine SECP521R1 curve point.
type PA = WAPoint SECP521R1 Fq Fr

-- Affine SECP521R1 curve is a Weierstrass affine curve.
instance WACurve SECP521R1 Fq Fr where
  gA_ = gA
  {-# INLINABLE gA_ #-}

-- | Jacobian SECP521R1 point.
type PJ = WJPoint SECP521R1 Fq Fr

-- Jacobian SECP521R1 curve is a Weierstrass Jacobian curve.
instance WJCurve SECP521R1 Fq Fr where
  gJ_ = gJ
  {-# INLINABLE gJ_ #-}

-- | Projective SECP521R1 point.
type PP = WPPoint SECP521R1 Fq Fr

-- Projective SECP521R1 curve is a Weierstrass projective curve.
instance WPCurve SECP521R1 Fq Fr where
  gP_ = gP
  {-# INLINABLE gP_ #-}

-------------------------------------------------------------------------------
-- Parameters
-------------------------------------------------------------------------------

-- | Coefficient @A@ of SECP521R1 curve.
_a :: Fq
_a = 0x1fffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffc
{-# INLINABLE _a #-}

-- | Coefficient @B@ of SECP521R1 curve.
_b :: Fq
_b = 0x51953eb9618e1c9a1f929a21a0b68540eea2da725b99b315f3b8b489918ef109e156193951ec7e937b1652c0bd3bb1bf073573df883d2c34f1ef451fd46b503f00
{-# INLINABLE _b #-}

-- | Cofactor of SECP521R1 curve.
_h :: Natural
_h = 0x1
{-# INLINABLE _h #-}

-- | Characteristic of SECP521R1 curve.
_q :: Natural
_q = 0x1ffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff
{-# INLINABLE _q #-}

-- | Order of SECP521R1 curve.
_r :: Natural
_r = 0x1fffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffa51868783bf2f966b7fcc0148f709a5d03bb5c9b8899c47aebb6fb71e91386409
{-# INLINABLE _r #-}

-- | Coordinate @X@ of SECP521R1 curve.
_x :: Fq
_x = 0xc6858e06b70404e9cd9e3ecb662395b4429c648139053fb521f828af606b4d3dbaa14b5e77efe75928fe1dc127a2ffa8de3348b3c1856a429bf97e7e31c2e5bd66
{-# INLINABLE _x #-}

-- | Coordinate @Y@ of SECP521R1 curve.
_y :: Fq
_y = 0x11839296a789a3bc0045c8a5fb42c7d1bd998f54449579b446817afbd17273e662c97ee72995ef42640c550b9013fad0761353c7086a272c24088be94769fd16650
{-# INLINABLE _y #-}

-- | Generator of affine SECP521R1 curve.
gA :: PA
gA = A _x _y
{-# INLINABLE gA #-}

-- | Generator of Jacobian SECP521R1 curve.
gJ :: PJ
gJ = J _x _y 1
{-# INLINABLE gJ #-}

-- | Generator of projective SECP521R1 curve.
gP :: PP
gP = P _x _y 1
{-# INLINABLE gP #-}
