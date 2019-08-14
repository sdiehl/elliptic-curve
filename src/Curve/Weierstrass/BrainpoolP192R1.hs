module Curve.Weierstrass.BrainpoolP192R1
  ( module Curve.Weierstrass
  , module Curve.Weierstrass.BrainpoolP192R1
  , Point(..)
  ) where

import Protolude

import PrimeField

import Curve.Weierstrass

-------------------------------------------------------------------------------
-- Types
-------------------------------------------------------------------------------

-- | BrainpoolP192R1 curve.
data BrainpoolP192R1

-- | Field of points of BrainpoolP192R1 curve.
type Fq = PrimeField 0xc302f41d932a36cda7a3463093d18db78fce476de1a86297

-- | Field of coefficients of BrainpoolP192R1 curve.
type Fr = PrimeField 0xc302f41d932a36cda7a3462f9e9e916b5be8f1029ac4acc1

-- | BrainpoolP192R1 curve is a Weierstrass curve.
instance Curve 'Weierstrass c BrainpoolP192R1 Fq Fr => WCurve c BrainpoolP192R1 Fq Fr where
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

-- | Affine BrainpoolP192R1 curve point.
type PA = WAPoint BrainpoolP192R1 Fq Fr

-- | Affine BrainpoolP192R1 curve is a Weierstrass affine curve.
instance WACurve BrainpoolP192R1 Fq Fr where
  gA_ = gA
  {-# INLINABLE gA_ #-}

-- | Jacobian BrainpoolP192R1 point.
type PJ = WJPoint BrainpoolP192R1 Fq Fr

-- | Jacobian BrainpoolP192R1 curve is a Weierstrass Jacobian curve.
instance WJCurve BrainpoolP192R1 Fq Fr where
  gJ_ = gJ
  {-# INLINABLE gJ_ #-}

-- | Projective BrainpoolP192R1 point.
type PP = WPPoint BrainpoolP192R1 Fq Fr

-- | Projective BrainpoolP192R1 curve is a Weierstrass projective curve.
instance WPCurve BrainpoolP192R1 Fq Fr where
  gP_ = gP
  {-# INLINABLE gP_ #-}

-------------------------------------------------------------------------------
-- Parameters
-------------------------------------------------------------------------------

-- | Coefficient @A@ of BrainpoolP192R1 curve.
_a :: Fq
_a = 0x6a91174076b1e0e19c39c031fe8685c1cae040e5c69a28ef
{-# INLINABLE _a #-}

-- | Coefficient @B@ of BrainpoolP192R1 curve.
_b :: Fq
_b = 0x469a28ef7c28cca3dc721d044f4496bcca7ef4146fbf25c9
{-# INLINABLE _b #-}

-- | Cofactor of BrainpoolP192R1 curve.
_h :: Integer
_h = 0x1
{-# INLINABLE _h #-}

-- | Characteristic of BrainpoolP192R1 curve.
_q :: Integer
_q = 0xc302f41d932a36cda7a3463093d18db78fce476de1a86297
{-# INLINABLE _q #-}

-- | Order of BrainpoolP192R1 curve.
_r :: Integer
_r = 0xc302f41d932a36cda7a3462f9e9e916b5be8f1029ac4acc1
{-# INLINABLE _r #-}

-- | Coordinate @X@ of BrainpoolP192R1 curve.
_x :: Fq
_x = 0xc0a0647eaab6a48753b033c56cb0f0900a2f5c4853375fd6
{-# INLINABLE _x #-}

-- | Coordinate @Y@ of BrainpoolP192R1 curve.
_y :: Fq
_y = 0x14b690866abd5bb88b5f4828c1490002e6773fa2fa299b8f
{-# INLINABLE _y #-}

-- | Generator of affine BrainpoolP192R1 curve.
gA :: PA
gA = A _x _y
{-# INLINABLE gA #-}

-- | Generator of Jacobian BrainpoolP192R1 curve.
gJ :: PJ
gJ = J _x _y 1
{-# INLINABLE gJ #-}

-- | Generator of projective BrainpoolP192R1 curve.
gP :: PP
gP = P _x _y 1
{-# INLINABLE gP #-}
