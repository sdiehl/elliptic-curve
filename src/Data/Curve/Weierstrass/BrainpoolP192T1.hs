module Data.Curve.Weierstrass.BrainpoolP192T1
  ( module Data.Curve.Weierstrass
  -- * BrainpoolP192T1 curve
  , module Data.Curve.Weierstrass.BrainpoolP192T1
  ) where

import Protolude

import Data.Field.Galois
import GHC.Natural (Natural)

import Data.Curve.Weierstrass
import Data.Curve.Weierstrass.Base (WCurve(..), WACurve(..), WJCurve(..), WPCurve(..))

-------------------------------------------------------------------------------
-- Types
-------------------------------------------------------------------------------

-- | BrainpoolP192T1 curve.
data BrainpoolP192T1

-- | Field of points of BrainpoolP192T1 curve.
type Fq = Prime 0xc302f41d932a36cda7a3463093d18db78fce476de1a86297

-- | Field of coefficients of BrainpoolP192T1 curve.
type Fr = Prime 0xc302f41d932a36cda7a3462f9e9e916b5be8f1029ac4acc1

-- BrainpoolP192T1 curve is a Weierstrass curve.
instance Curve 'Weierstrass c BrainpoolP192T1 Fq Fr => WCurve c BrainpoolP192T1 Fq Fr where
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

-- | Affine BrainpoolP192T1 curve point.
type PA = WAPoint BrainpoolP192T1 Fq Fr

-- Affine BrainpoolP192T1 curve is a Weierstrass affine curve.
instance WACurve BrainpoolP192T1 Fq Fr where
  gA_ = gA
  {-# INLINABLE gA_ #-}

-- | Jacobian BrainpoolP192T1 point.
type PJ = WJPoint BrainpoolP192T1 Fq Fr

-- Jacobian BrainpoolP192T1 curve is a Weierstrass Jacobian curve.
instance WJCurve BrainpoolP192T1 Fq Fr where
  gJ_ = gJ
  {-# INLINABLE gJ_ #-}

-- | Projective BrainpoolP192T1 point.
type PP = WPPoint BrainpoolP192T1 Fq Fr

-- Projective BrainpoolP192T1 curve is a Weierstrass projective curve.
instance WPCurve BrainpoolP192T1 Fq Fr where
  gP_ = gP
  {-# INLINABLE gP_ #-}

-------------------------------------------------------------------------------
-- Parameters
-------------------------------------------------------------------------------

-- | Coefficient @A@ of BrainpoolP192T1 curve.
_a :: Fq
_a = 0xc302f41d932a36cda7a3463093d18db78fce476de1a86294
{-# INLINABLE _a #-}

-- | Coefficient @B@ of BrainpoolP192T1 curve.
_b :: Fq
_b = 0x13d56ffaec78681e68f9deb43b35bec2fb68542e27897b79
{-# INLINABLE _b #-}

-- | Cofactor of BrainpoolP192T1 curve.
_h :: Natural
_h = 0x1
{-# INLINABLE _h #-}

-- | Characteristic of BrainpoolP192T1 curve.
_q :: Natural
_q = 0xc302f41d932a36cda7a3463093d18db78fce476de1a86297
{-# INLINABLE _q #-}

-- | Order of BrainpoolP192T1 curve.
_r :: Natural
_r = 0xc302f41d932a36cda7a3462f9e9e916b5be8f1029ac4acc1
{-# INLINABLE _r #-}

-- | Coordinate @X@ of BrainpoolP192T1 curve.
_x :: Fq
_x = 0x3ae9e58c82f63c30282e1fe7bbf43fa72c446af6f4618129
{-# INLINABLE _x #-}

-- | Coordinate @Y@ of BrainpoolP192T1 curve.
_y :: Fq
_y = 0x97e2c5667c2223a902ab5ca449d0084b7e5b3de7ccc01c9
{-# INLINABLE _y #-}

-- | Generator of affine BrainpoolP192T1 curve.
gA :: PA
gA = A _x _y
{-# INLINABLE gA #-}

-- | Generator of Jacobian BrainpoolP192T1 curve.
gJ :: PJ
gJ = J _x _y 1
{-# INLINABLE gJ #-}

-- | Generator of projective BrainpoolP192T1 curve.
gP :: PP
gP = P _x _y 1
{-# INLINABLE gP #-}
