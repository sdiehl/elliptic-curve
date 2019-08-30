module Data.Curve.Weierstrass.SECP192R1
  ( module Data.Curve.Weierstrass
  -- * SECP192R1 curve
  , module Data.Curve.Weierstrass.SECP192R1
  ) where

import Protolude

import Data.Field.Galois
import GHC.Natural (Natural)

import Data.Curve.Weierstrass
import Data.Curve.Weierstrass.Base (WCurve(..), WACurve(..), WJCurve(..), WPCurve(..))

-------------------------------------------------------------------------------
-- Types
-------------------------------------------------------------------------------

-- | SECP192R1 curve.
data SECP192R1

-- | Field of points of SECP192R1 curve.
type Fq = Prime 0xfffffffffffffffffffffffffffffffeffffffffffffffff

-- | Field of coefficients of SECP192R1 curve.
type Fr = Prime 0xffffffffffffffffffffffff99def836146bc9b1b4d22831

-- SECP192R1 curve is a Weierstrass curve.
instance Curve 'Weierstrass c SECP192R1 Fq Fr => WCurve c SECP192R1 Fq Fr where
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

-- | Affine SECP192R1 curve point.
type PA = WAPoint SECP192R1 Fq Fr

-- Affine SECP192R1 curve is a Weierstrass affine curve.
instance WACurve SECP192R1 Fq Fr where
  gA_ = gA
  {-# INLINABLE gA_ #-}

-- | Jacobian SECP192R1 point.
type PJ = WJPoint SECP192R1 Fq Fr

-- Jacobian SECP192R1 curve is a Weierstrass Jacobian curve.
instance WJCurve SECP192R1 Fq Fr where
  gJ_ = gJ
  {-# INLINABLE gJ_ #-}

-- | Projective SECP192R1 point.
type PP = WPPoint SECP192R1 Fq Fr

-- Projective SECP192R1 curve is a Weierstrass projective curve.
instance WPCurve SECP192R1 Fq Fr where
  gP_ = gP
  {-# INLINABLE gP_ #-}

-------------------------------------------------------------------------------
-- Parameters
-------------------------------------------------------------------------------

-- | Coefficient @A@ of SECP192R1 curve.
_a :: Fq
_a = 0xfffffffffffffffffffffffffffffffefffffffffffffffc
{-# INLINABLE _a #-}

-- | Coefficient @B@ of SECP192R1 curve.
_b :: Fq
_b = 0x64210519e59c80e70fa7e9ab72243049feb8deecc146b9b1
{-# INLINABLE _b #-}

-- | Cofactor of SECP192R1 curve.
_h :: Natural
_h = 0x1
{-# INLINABLE _h #-}

-- | Characteristic of SECP192R1 curve.
_q :: Natural
_q = 0xfffffffffffffffffffffffffffffffeffffffffffffffff
{-# INLINABLE _q #-}

-- | Order of SECP192R1 curve.
_r :: Natural
_r = 0xffffffffffffffffffffffff99def836146bc9b1b4d22831
{-# INLINABLE _r #-}

-- | Coordinate @X@ of SECP192R1 curve.
_x :: Fq
_x = 0x188da80eb03090f67cbf20eb43a18800f4ff0afd82ff1012
{-# INLINABLE _x #-}

-- | Coordinate @Y@ of SECP192R1 curve.
_y :: Fq
_y = 0x7192b95ffc8da78631011ed6b24cdd573f977a11e794811
{-# INLINABLE _y #-}

-- | Generator of affine SECP192R1 curve.
gA :: PA
gA = A _x _y
{-# INLINABLE gA #-}

-- | Generator of Jacobian SECP192R1 curve.
gJ :: PJ
gJ = J _x _y 1
{-# INLINABLE gJ #-}

-- | Generator of projective SECP192R1 curve.
gP :: PP
gP = P _x _y 1
{-# INLINABLE gP #-}
