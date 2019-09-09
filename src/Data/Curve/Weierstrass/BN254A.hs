module Data.Curve.Weierstrass.BN254A
  ( module Data.Curve.Weierstrass
  -- * BN254A curve
  , module Data.Curve.Weierstrass.BN254A
  ) where

import Protolude

import Data.Field.Galois
import GHC.Natural (Natural)

import Data.Curve.Weierstrass

-------------------------------------------------------------------------------
-- Types
-------------------------------------------------------------------------------

-- | BN254A curve.
data BN254A

-- | Field of points of BN254A curve.
type Fq = Prime Q
type Q = 0x2370fb049d410fbe4e761a9886e502417d023f40180000017e80600000000001

-- | Field of coefficients of BN254A curve.
type Fr = Prime R
type R = 0x2370fb049d410fbe4e761a9886e502411dc1af70120000017e80600000000001

-- BN254A curve is a Weierstrass curve.
instance Curve 'Weierstrass c BN254A Fq Fr => WCurve c BN254A Fq Fr where
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

-- | Affine BN254A curve point.
type PA = WAPoint BN254A Fq Fr

-- Affine BN254A curve is a Weierstrass affine curve.
instance WACurve BN254A Fq Fr where
  gA_ = gA
  {-# INLINABLE gA_ #-}

-- | Jacobian BN254A point.
type PJ = WJPoint BN254A Fq Fr

-- Jacobian BN254A curve is a Weierstrass Jacobian curve.
instance WJCurve BN254A Fq Fr where
  gJ_ = gJ
  {-# INLINABLE gJ_ #-}

-- | Projective BN254A point.
type PP = WPPoint BN254A Fq Fr

-- Projective BN254A curve is a Weierstrass projective curve.
instance WPCurve BN254A Fq Fr where
  gP_ = gP
  {-# INLINABLE gP_ #-}

-------------------------------------------------------------------------------
-- Parameters
-------------------------------------------------------------------------------

-- | Coefficient @A@ of BN254A curve.
_a :: Fq
_a = 0x0
{-# INLINABLE _a #-}

-- | Coefficient @B@ of BN254A curve.
_b :: Fq
_b = 0x5
{-# INLINABLE _b #-}

-- | Cofactor of BN254A curve.
_h :: Natural
_h = 0x1
{-# INLINABLE _h #-}

-- | Characteristic of BN254A curve.
_q :: Natural
_q = 0x2370fb049d410fbe4e761a9886e502417d023f40180000017e80600000000001
{-# INLINABLE _q #-}

-- | Order of BN254A curve.
_r :: Natural
_r = 0x2370fb049d410fbe4e761a9886e502411dc1af70120000017e80600000000001
{-# INLINABLE _r #-}

-- | Coordinate @X@ of BN254A curve.
_x :: Fq
_x = 0x1
{-# INLINABLE _x #-}

-- | Coordinate @Y@ of BN254A curve.
_y :: Fq
_y = 0xd45589b158faaf6ab0e4ad38d998e9982e7ff63964ee1460342a592677cccb0
{-# INLINABLE _y #-}

-- | Generator of affine BN254A curve.
gA :: PA
gA = A _x _y
{-# INLINABLE gA #-}

-- | Generator of Jacobian BN254A curve.
gJ :: PJ
gJ = J _x _y 1
{-# INLINABLE gJ #-}

-- | Generator of projective BN254A curve.
gP :: PP
gP = P _x _y 1
{-# INLINABLE gP #-}
