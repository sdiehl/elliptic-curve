module Data.Curve.Weierstrass.BN254BT
  ( module Data.Curve.Weierstrass
  -- * BN254BT curve
  , module Data.Curve.Weierstrass.BN254BT
  ) where

import Protolude

import Data.Field.Galois
import GHC.Natural (Natural)

import Data.Curve.Weierstrass
import Data.Curve.Weierstrass.Base (WCurve(..), WACurve(..), WJCurve(..), WPCurve(..))
import Data.Curve.Weierstrass.BN254B (Fq)

-------------------------------------------------------------------------------
-- Types
-------------------------------------------------------------------------------

-- | BN254BT curve.
data BN254BT

-- | Field of points of BN254BT curve.
data PolynomialU
instance IrreducibleMonic Fq PolynomialU where
  split _ = X2 + 1
  {-# INLINABLE split #-}
type Fq2 = Extension Fq PolynomialU

-- | Field of coefficients of BN254BT curve.
type Fr = Prime 0x2523648240000001ba344d8000000007ff9f800000000010a10000000000000d

-- BN254BT curve is a Weierstrass curve.
instance Curve 'Weierstrass c BN254BT Fq2 Fr => WCurve c BN254BT Fq2 Fr where
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

-- | Affine BN254BT curve point.
type PA = WAPoint BN254BT Fq2 Fr

-- Affine BN254BT curve is a Weierstrass affine curve.
instance WACurve BN254BT Fq2 Fr where
  gA_ = gA
  {-# INLINABLE gA_ #-}

-- | Jacobian BN254BT point.
type PJ = WJPoint BN254BT Fq2 Fr

-- Jacobian BN254BT curve is a Weierstrass Jacobian curve.
instance WJCurve BN254BT Fq2 Fr where
  gJ_ = gJ
  {-# INLINABLE gJ_ #-}

-- | Projective BN254BT point.
type PP = WPPoint BN254BT Fq2 Fr

-- Projective BN254BT curve is a Weierstrass projective curve.
instance WPCurve BN254BT Fq2 Fr where
  gP_ = gP
  {-# INLINABLE gP_ #-}

-------------------------------------------------------------------------------
-- Parameters
-------------------------------------------------------------------------------

-- | Coefficient @A@ of BN254BT curve.
_a :: Fq2
_a = toE' [
          ]
{-# INLINABLE _a #-}

-- | Coefficient @B@ of BN254BT curve.
_b :: Fq2
_b = toE' [ 0x1
          , 0x2523648240000001ba344d80000000086121000000000013a700000000000012
          ]
{-# INLINABLE _b #-}

-- | Cofactor of BN254BT curve.
_h :: Natural
_h = 0x2523648240000001ba344d8000000008c2a2800000000016ad00000000000019
{-# INLINABLE _h #-}

-- | Characteristic of BN254BT curve.
_q :: Natural
_q = 0x2523648240000001ba344d80000000086121000000000013a700000000000013
{-# INLINABLE _q #-}

-- | Order of BN254BT curve.
_r :: Natural
_r = 0x2523648240000001ba344d8000000007ff9f800000000010a10000000000000d
{-# INLINABLE _r #-}

-- | Coordinate @X@ of BN254BT curve.
_x :: Fq2
_x = toE' [ 0x61a10bb519eb62feb8d8c7e8c61edb6a4648bbb4898bf0d91ee4224c803fb2b
          , 0x516aaf9ba737833310aa78c5982aa5b1f4d746bae3784b70d8c34c1e7d54cf3
          ]
{-# INLINABLE _x #-}

-- | Coordinate @Y@ of BN254BT curve.
_y :: Fq2
_y = toE' [ 0x21897a06baf93439a90e096698c822329bd0ae6bdbe09bd19f0e07891cd2b9a
          , 0xebb2b0e7c8b15268f6d4456f5f38d37b09006ffd739c9578a2d1aec6b3ace9b
          ]
{-# INLINABLE _y #-}

-- | Generator of affine BN254BT curve.
gA :: PA
gA = A _x _y
{-# INLINABLE gA #-}

-- | Generator of Jacobian BN254BT curve.
gJ :: PJ
gJ = J _x _y 1
{-# INLINABLE gJ #-}

-- | Generator of projective BN254BT curve.
gP :: PP
gP = P _x _y 1
{-# INLINABLE gP #-}
