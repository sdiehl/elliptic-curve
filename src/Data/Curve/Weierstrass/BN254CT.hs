module Data.Curve.Weierstrass.BN254CT
  ( module Data.Curve.Weierstrass
  -- * BN254CT curve
  , module Data.Curve.Weierstrass.BN254CT
  ) where

import Protolude

import Data.Field.Galois
import GHC.Natural (Natural)

import Data.Curve.Weierstrass
import Data.Curve.Weierstrass.BN254C (BN254C, Fq)

-------------------------------------------------------------------------------
-- Types
-------------------------------------------------------------------------------

-- | Field of points of BN254CT curve.
type Fq2 = Extension U Fq
data U
instance IrreducibleMonic U Fq where
  poly _ = X2 + 1
  {-# INLINABLE poly #-}

-- | Field of coefficients of BN254CT curve.
type Fr = Prime R
type R = 0x240120db6517014efa0bab3696f8d5f00e88d43492b2cb363a75777e8d30210d

-- BN254CT curve is a Weierstrass curve.
instance Curve 'Weierstrass c BN254C Fq2 Fr => WCurve c BN254C Fq2 Fr where
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

-- | Affine BN254CT curve point.
type PA = WAPoint BN254C Fq2 Fr

-- Affine BN254CT curve is a Weierstrass affine curve.
instance WACurve BN254C Fq2 Fr where
  gA_ = gA
  {-# INLINABLE gA_ #-}

-- | Jacobian BN254CT point.
type PJ = WJPoint BN254C Fq2 Fr

-- Jacobian BN254CT curve is a Weierstrass Jacobian curve.
instance WJCurve BN254C Fq2 Fr where
  gJ_ = gJ
  {-# INLINABLE gJ_ #-}

-- | Projective BN254CT point.
type PP = WPPoint BN254C Fq2 Fr

-- Projective BN254CT curve is a Weierstrass projective curve.
instance WPCurve BN254C Fq2 Fr where
  gP_ = gP
  {-# INLINABLE gP_ #-}

-------------------------------------------------------------------------------
-- Parameters
-------------------------------------------------------------------------------

-- | Coefficient @A@ of BN254CT curve.
_a :: Fq2
_a = toE' [
          ]
{-# INLINABLE _a #-}

-- | Coefficient @B@ of BN254CT curve.
_b :: Fq2
_b = toE' [ 0x1
          , 0x240120db6517014efa0bab3696f8d5f06e8a555614f464babe9dbbfeeeb4a712
          ]
{-# INLINABLE _b #-}

-- | Cofactor of BN254CT curve.
_h :: Natural
_h = 0x240120db6517014efa0bab3696f8d5f0ce8bd6779735fe3f42c6007f50392d19
{-# INLINABLE _h #-}

-- | Characteristic of BN254CT curve.
_q :: Natural
_q = 0x240120db6517014efa0bab3696f8d5f06e8a555614f464babe9dbbfeeeb4a713
{-# INLINABLE _q #-}

-- | Order of BN254CT curve.
_r :: Natural
_r = 0x240120db6517014efa0bab3696f8d5f00e88d43492b2cb363a75777e8d30210d
{-# INLINABLE _r #-}

-- | Coordinate @X@ of BN254CT curve.
_x :: Fq2
_x = toE' [ 0x571af2ea9666eb2a53f3fb837172bdd809c03a95c5870f34a8cb340220bf9c0
          , 0xf71abb712a9e6e12c07b58bc01f2f994c3b5a1531cf96609b838e5ccf05bc71
          ]
{-# INLINABLE _x #-}

-- | Coordinate @Y@ of BN254CT curve.
_y :: Fq2
_y = toE' [ 0xb88822fe134c1695b21419bb1ab9732f707701046a2e6ff3ad10f3c70284b93
          , 0x1659b723676b5af5231fb045b3d822c0de6fcaab171bad9c8951afc800a26775
          ]
{-# INLINABLE _y #-}

-- | Generator of affine BN254CT curve.
gA :: PA
gA = A _x _y
{-# INLINABLE gA #-}

-- | Generator of Jacobian BN254CT curve.
gJ :: PJ
gJ = J _x _y 1
{-# INLINABLE gJ #-}

-- | Generator of projective BN254CT curve.
gP :: PP
gP = P _x _y 1
{-# INLINABLE gP #-}
