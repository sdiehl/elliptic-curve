module Curve.Weierstrass.BN254B
  ( module Curve.Weierstrass
  , module Curve.Weierstrass.BN254B
  ) where

import Protolude

import PrimeField

import Curve.Weierstrass

-------------------------------------------------------------------------------
-- Types
-------------------------------------------------------------------------------

-- | BN254B curve.
data BN254B

-- | Field of points of BN254B curve.
type Fq = PrimeField 0x2523648240000001ba344d80000000086121000000000013a700000000000013

-- | Field of coefficients of BN254B curve.
type Fr = PrimeField 0x2523648240000001ba344d8000000007ff9f800000000010a10000000000000d

-- | BN254B curve is a Weierstrass curve.
instance Curve 'Weierstrass c BN254B Fq Fr => WCurve c BN254B Fq Fr where
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

-- | Affine BN254B curve point.
type PA = WAPoint BN254B Fq Fr

-- | Affine BN254B curve is a Weierstrass affine curve.
instance WACurve BN254B Fq Fr where
  gA_ = gA
  {-# INLINABLE gA_ #-}

-- | Jacobian BN254B point.
type PJ = WJPoint BN254B Fq Fr

-- | Jacobian BN254B curve is a Weierstrass Jacobian curve.
instance WJCurve BN254B Fq Fr where
  gJ_ = gJ
  {-# INLINABLE gJ_ #-}

-- | Projective BN254B point.
type PP = WPPoint BN254B Fq Fr

-- | Projective BN254B curve is a Weierstrass projective curve.
instance WPCurve BN254B Fq Fr where
  gP_ = gP
  {-# INLINABLE gP_ #-}

-------------------------------------------------------------------------------
-- Parameters
-------------------------------------------------------------------------------

-- | Coefficient @A@ of BN254B curve.
_a :: Fq
_a = 0x0
{-# INLINABLE _a #-}

-- | Coefficient @B@ of BN254B curve.
_b :: Fq
_b = 0x2
{-# INLINABLE _b #-}

-- | Cofactor of BN254B curve.
_h :: Integer
_h = 0x1
{-# INLINABLE _h #-}

-- | Characteristic of BN254B curve.
_q :: Integer
_q = 0x2523648240000001ba344d80000000086121000000000013a700000000000013
{-# INLINABLE _q #-}

-- | Order of BN254B curve.
_r :: Integer
_r = 0x2523648240000001ba344d8000000007ff9f800000000010a10000000000000d
{-# INLINABLE _r #-}

-- | Coordinate @X@ of BN254B curve.
_x :: Fq
_x = 0x2523648240000001ba344d80000000086121000000000013a700000000000012
{-# INLINABLE _x #-}

-- | Coordinate @Y@ of BN254B curve.
_y :: Fq
_y = 0x1
{-# INLINABLE _y #-}

-- | Generator of affine BN254B curve.
gA :: PA
gA = A _x _y
{-# INLINABLE gA #-}

-- | Generator of Jacobian BN254B curve.
gJ :: PJ
gJ = J _x _y 1
{-# INLINABLE gJ #-}

-- | Generator of projective BN254B curve.
gP :: PP
gP = P _x _y 1
{-# INLINABLE gP #-}
