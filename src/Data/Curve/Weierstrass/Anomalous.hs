module Data.Curve.Weierstrass.Anomalous
  ( module Data.Curve.Weierstrass
  , module Data.Curve.Weierstrass.Anomalous
  , Point(..)
  ) where

import Protolude

import Data.Field.Galois
import GHC.Natural (Natural)

import Data.Curve.Weierstrass

-------------------------------------------------------------------------------
-- Types
-------------------------------------------------------------------------------

-- | Anomalous curve.
data Anomalous

-- | Field of points of Anomalous curve.
type Fq = Prime 0xb0000000000000000000000953000000000000000000001f9d7

-- | Field of coefficients of Anomalous curve.
type Fr = Prime 0xb0000000000000000000000953000000000000000000001f9d7

-- | Anomalous curve is a Weierstrass curve.
instance Curve 'Weierstrass c Anomalous Fq Fr => WCurve c Anomalous Fq Fr where
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

-- | Affine Anomalous curve point.
type PA = WAPoint Anomalous Fq Fr

-- | Affine Anomalous curve is a Weierstrass affine curve.
instance WACurve Anomalous Fq Fr where
  gA_ = gA
  {-# INLINABLE gA_ #-}

-- | Jacobian Anomalous point.
type PJ = WJPoint Anomalous Fq Fr

-- | Jacobian Anomalous curve is a Weierstrass Jacobian curve.
instance WJCurve Anomalous Fq Fr where
  gJ_ = gJ
  {-# INLINABLE gJ_ #-}

-- | Projective Anomalous point.
type PP = WPPoint Anomalous Fq Fr

-- | Projective Anomalous curve is a Weierstrass projective curve.
instance WPCurve Anomalous Fq Fr where
  gP_ = gP
  {-# INLINABLE gP_ #-}

-------------------------------------------------------------------------------
-- Parameters
-------------------------------------------------------------------------------

-- | Coefficient @A@ of Anomalous curve.
_a :: Fq
_a = 0x98d0fac687d6343eb1a1f595283eb1a1f58d0fac687d635f5e4
{-# INLINABLE _a #-}

-- | Coefficient @B@ of Anomalous curve.
_b :: Fq
_b = 0x4a1f58d0fac687d6343eb1a5e2d6343eb1a1f58d0fac688ab3f
{-# INLINABLE _b #-}

-- | Cofactor of Anomalous curve.
_h :: Natural
_h = 0x1
{-# INLINABLE _h #-}

-- | Characteristic of Anomalous curve.
_q :: Natural
_q = 0xb0000000000000000000000953000000000000000000001f9d7
{-# INLINABLE _q #-}

-- | Order of Anomalous curve.
_r :: Natural
_r = 0xb0000000000000000000000953000000000000000000001f9d7
{-# INLINABLE _r #-}

-- | Coordinate @X@ of Anomalous curve.
_x :: Fq
_x = 0x101efb35fd1963c4871a2d17edaafa7e249807f58f8705126c6
{-# INLINABLE _x #-}

-- | Coordinate @Y@ of Anomalous curve.
_y :: Fq
_y = 0x22389a3954375834304ba1d509a97de6c07148ea7f5951b20e7
{-# INLINABLE _y #-}

-- | Generator of affine Anomalous curve.
gA :: PA
gA = A _x _y
{-# INLINABLE gA #-}

-- | Generator of Jacobian Anomalous curve.
gJ :: PJ
gJ = J _x _y 1
{-# INLINABLE gJ #-}

-- | Generator of projective Anomalous curve.
gP :: PP
gP = P _x _y 1
{-# INLINABLE gP #-}
