module Data.Curve.Weierstrass.SECP128R2
  ( module Data.Curve.Weierstrass
  -- * SECP128R2 curve
  , module Data.Curve.Weierstrass.SECP128R2
  ) where

import Protolude

import Data.Field.Galois
import GHC.Natural (Natural)

import Data.Curve.Weierstrass
import Data.Curve.Weierstrass.Base (WCurve(..), WACurve(..), WJCurve(..), WPCurve(..))

-------------------------------------------------------------------------------
-- Types
-------------------------------------------------------------------------------

-- | SECP128R2 curve.
data SECP128R2

-- | Field of points of SECP128R2 curve.
type Fq = Prime Q
type Q = 0xfffffffdffffffffffffffffffffffff

-- | Field of coefficients of SECP128R2 curve.
type Fr = Prime R
type R = 0x3fffffff7fffffffbe0024720613b5a3

-- SECP128R2 curve is a Weierstrass curve.
instance Curve 'Weierstrass c SECP128R2 Fq Fr => WCurve c SECP128R2 Fq Fr where
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

-- | Affine SECP128R2 curve point.
type PA = WAPoint SECP128R2 Fq Fr

-- Affine SECP128R2 curve is a Weierstrass affine curve.
instance WACurve SECP128R2 Fq Fr where
  gA_ = gA
  {-# INLINABLE gA_ #-}

-- | Jacobian SECP128R2 point.
type PJ = WJPoint SECP128R2 Fq Fr

-- Jacobian SECP128R2 curve is a Weierstrass Jacobian curve.
instance WJCurve SECP128R2 Fq Fr where
  gJ_ = gJ
  {-# INLINABLE gJ_ #-}

-- | Projective SECP128R2 point.
type PP = WPPoint SECP128R2 Fq Fr

-- Projective SECP128R2 curve is a Weierstrass projective curve.
instance WPCurve SECP128R2 Fq Fr where
  gP_ = gP
  {-# INLINABLE gP_ #-}

-------------------------------------------------------------------------------
-- Parameters
-------------------------------------------------------------------------------

-- | Coefficient @A@ of SECP128R2 curve.
_a :: Fq
_a = 0xd6031998d1b3bbfebf59cc9bbff9aee1
{-# INLINABLE _a #-}

-- | Coefficient @B@ of SECP128R2 curve.
_b :: Fq
_b = 0x5eeefca380d02919dc2c6558bb6d8a5d
{-# INLINABLE _b #-}

-- | Cofactor of SECP128R2 curve.
_h :: Natural
_h = 0x4
{-# INLINABLE _h #-}

-- | Characteristic of SECP128R2 curve.
_q :: Natural
_q = 0xfffffffdffffffffffffffffffffffff
{-# INLINABLE _q #-}

-- | Order of SECP128R2 curve.
_r :: Natural
_r = 0x3fffffff7fffffffbe0024720613b5a3
{-# INLINABLE _r #-}

-- | Coordinate @X@ of SECP128R2 curve.
_x :: Fq
_x = 0x7b6aa5d85e572983e6fb32a7cdebc140
{-# INLINABLE _x #-}

-- | Coordinate @Y@ of SECP128R2 curve.
_y :: Fq
_y = 0x27b6916a894d3aee7106fe805fc34b44
{-# INLINABLE _y #-}

-- | Generator of affine SECP128R2 curve.
gA :: PA
gA = A _x _y
{-# INLINABLE gA #-}

-- | Generator of Jacobian SECP128R2 curve.
gJ :: PJ
gJ = J _x _y 1
{-# INLINABLE gJ #-}

-- | Generator of projective SECP128R2 curve.
gP :: PP
gP = P _x _y 1
{-# INLINABLE gP #-}
