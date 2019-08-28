module Data.Curve.Weierstrass.BrainpoolP512R1
  ( module Data.Curve.Weierstrass
  , module Data.Curve.Weierstrass.BrainpoolP512R1
  , Point(..)
  ) where

import Protolude

import Data.Field.Galois

import Data.Curve.Weierstrass

-------------------------------------------------------------------------------
-- Types
-------------------------------------------------------------------------------

-- | BrainpoolP512R1 curve.
data BrainpoolP512R1

-- | Field of points of BrainpoolP512R1 curve.
type Fq = Prime 0xaadd9db8dbe9c48b3fd4e6ae33c9fc07cb308db3b3c9d20ed6639cca703308717d4d9b009bc66842aecda12ae6a380e62881ff2f2d82c68528aa6056583a48f3

-- | Field of coefficients of BrainpoolP512R1 curve.
type Fr = Prime 0xaadd9db8dbe9c48b3fd4e6ae33c9fc07cb308db3b3c9d20ed6639cca70330870553e5c414ca92619418661197fac10471db1d381085ddaddb58796829ca90069

-- | BrainpoolP512R1 curve is a Weierstrass curve.
instance Curve 'Weierstrass c BrainpoolP512R1 Fq Fr => WCurve c BrainpoolP512R1 Fq Fr where
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

-- | Affine BrainpoolP512R1 curve point.
type PA = WAPoint BrainpoolP512R1 Fq Fr

-- | Affine BrainpoolP512R1 curve is a Weierstrass affine curve.
instance WACurve BrainpoolP512R1 Fq Fr where
  gA_ = gA
  {-# INLINABLE gA_ #-}

-- | Jacobian BrainpoolP512R1 point.
type PJ = WJPoint BrainpoolP512R1 Fq Fr

-- | Jacobian BrainpoolP512R1 curve is a Weierstrass Jacobian curve.
instance WJCurve BrainpoolP512R1 Fq Fr where
  gJ_ = gJ
  {-# INLINABLE gJ_ #-}

-- | Projective BrainpoolP512R1 point.
type PP = WPPoint BrainpoolP512R1 Fq Fr

-- | Projective BrainpoolP512R1 curve is a Weierstrass projective curve.
instance WPCurve BrainpoolP512R1 Fq Fr where
  gP_ = gP
  {-# INLINABLE gP_ #-}

-------------------------------------------------------------------------------
-- Parameters
-------------------------------------------------------------------------------

-- | Coefficient @A@ of BrainpoolP512R1 curve.
_a :: Fq
_a = 0x7830a3318b603b89e2327145ac234cc594cbdd8d3df91610a83441caea9863bc2ded5d5aa8253aa10a2ef1c98b9ac8b57f1117a72bf2c7b9e7c1ac4d77fc94ca
{-# INLINABLE _a #-}

-- | Coefficient @B@ of BrainpoolP512R1 curve.
_b :: Fq
_b = 0x3df91610a83441caea9863bc2ded5d5aa8253aa10a2ef1c98b9ac8b57f1117a72bf2c7b9e7c1ac4d77fc94cadc083e67984050b75ebae5dd2809bd638016f723
{-# INLINABLE _b #-}

-- | Cofactor of BrainpoolP512R1 curve.
_h :: Natural
_h = 0x1
{-# INLINABLE _h #-}

-- | Characteristic of BrainpoolP512R1 curve.
_q :: Natural
_q = 0xaadd9db8dbe9c48b3fd4e6ae33c9fc07cb308db3b3c9d20ed6639cca703308717d4d9b009bc66842aecda12ae6a380e62881ff2f2d82c68528aa6056583a48f3
{-# INLINABLE _q #-}

-- | Order of BrainpoolP512R1 curve.
_r :: Natural
_r = 0xaadd9db8dbe9c48b3fd4e6ae33c9fc07cb308db3b3c9d20ed6639cca70330870553e5c414ca92619418661197fac10471db1d381085ddaddb58796829ca90069
{-# INLINABLE _r #-}

-- | Coordinate @X@ of BrainpoolP512R1 curve.
_x :: Fq
_x = 0x81aee4bdd82ed9645a21322e9c4c6a9385ed9f70b5d916c1b43b62eef4d0098eff3b1f78e2d0d48d50d1687b93b97d5f7c6d5047406a5e688b352209bcb9f822
{-# INLINABLE _x #-}

-- | Coordinate @Y@ of BrainpoolP512R1 curve.
_y :: Fq
_y = 0x7dde385d566332ecc0eabfa9cf7822fdf209f70024a57b1aa000c55b881f8111b2dcde494a5f485e5bca4bd88a2763aed1ca2b2fa8f0540678cd1e0f3ad80892
{-# INLINABLE _y #-}

-- | Generator of affine BrainpoolP512R1 curve.
gA :: PA
gA = A _x _y
{-# INLINABLE gA #-}

-- | Generator of Jacobian BrainpoolP512R1 curve.
gJ :: PJ
gJ = J _x _y 1
{-# INLINABLE gJ #-}

-- | Generator of projective BrainpoolP512R1 curve.
gP :: PP
gP = P _x _y 1
{-# INLINABLE gP #-}
