module Data.Curve.Weierstrass.BrainpoolP320T1
  ( module Data.Curve.Weierstrass
  -- * BrainpoolP320T1 curve
  , module Data.Curve.Weierstrass.BrainpoolP320T1
  ) where

import Protolude

import Data.Field.Galois
import GHC.Natural (Natural)

import Data.Curve.Weierstrass
import Data.Curve.Weierstrass.Base (WCurve(..), WACurve(..), WJCurve(..), WPCurve(..))

-------------------------------------------------------------------------------
-- Types
-------------------------------------------------------------------------------

-- | BrainpoolP320T1 curve.
data BrainpoolP320T1

-- | Field of points of BrainpoolP320T1 curve.
type Fq = Prime Q
type Q = 0xd35e472036bc4fb7e13c785ed201e065f98fcfa6f6f40def4f92b9ec7893ec28fcd412b1f1b32e27

-- | Field of coefficients of BrainpoolP320T1 curve.
type Fr = Prime R
type R = 0xd35e472036bc4fb7e13c785ed201e065f98fcfa5b68f12a32d482ec7ee8658e98691555b44c59311

-- BrainpoolP320T1 curve is a Weierstrass curve.
instance Curve 'Weierstrass c BrainpoolP320T1 Fq Fr => WCurve c BrainpoolP320T1 Fq Fr where
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

-- | Affine BrainpoolP320T1 curve point.
type PA = WAPoint BrainpoolP320T1 Fq Fr

-- Affine BrainpoolP320T1 curve is a Weierstrass affine curve.
instance WACurve BrainpoolP320T1 Fq Fr where
  gA_ = gA
  {-# INLINABLE gA_ #-}

-- | Jacobian BrainpoolP320T1 point.
type PJ = WJPoint BrainpoolP320T1 Fq Fr

-- Jacobian BrainpoolP320T1 curve is a Weierstrass Jacobian curve.
instance WJCurve BrainpoolP320T1 Fq Fr where
  gJ_ = gJ
  {-# INLINABLE gJ_ #-}

-- | Projective BrainpoolP320T1 point.
type PP = WPPoint BrainpoolP320T1 Fq Fr

-- Projective BrainpoolP320T1 curve is a Weierstrass projective curve.
instance WPCurve BrainpoolP320T1 Fq Fr where
  gP_ = gP
  {-# INLINABLE gP_ #-}

-------------------------------------------------------------------------------
-- Parameters
-------------------------------------------------------------------------------

-- | Coefficient @A@ of BrainpoolP320T1 curve.
_a :: Fq
_a = 0xd35e472036bc4fb7e13c785ed201e065f98fcfa6f6f40def4f92b9ec7893ec28fcd412b1f1b32e24
{-# INLINABLE _a #-}

-- | Coefficient @B@ of BrainpoolP320T1 curve.
_b :: Fq
_b = 0xa7f561e038eb1ed560b3d147db782013064c19f27ed27c6780aaf77fb8a547ceb5b4fef422340353
{-# INLINABLE _b #-}

-- | Cofactor of BrainpoolP320T1 curve.
_h :: Natural
_h = 0x1
{-# INLINABLE _h #-}

-- | Characteristic of BrainpoolP320T1 curve.
_q :: Natural
_q = 0xd35e472036bc4fb7e13c785ed201e065f98fcfa6f6f40def4f92b9ec7893ec28fcd412b1f1b32e27
{-# INLINABLE _q #-}

-- | Order of BrainpoolP320T1 curve.
_r :: Natural
_r = 0xd35e472036bc4fb7e13c785ed201e065f98fcfa5b68f12a32d482ec7ee8658e98691555b44c59311
{-# INLINABLE _r #-}

-- | Coordinate @X@ of BrainpoolP320T1 curve.
_x :: Fq
_x = 0x925be9fb01afc6fb4d3e7d4990010f813408ab106c4f09cb7ee07868cc136fff3357f624a21bed52
{-# INLINABLE _x #-}

-- | Coordinate @Y@ of BrainpoolP320T1 curve.
_y :: Fq
_y = 0x63ba3a7a27483ebf6671dbef7abb30ebee084e58a0b077ad42a5a0989d1ee71b1b9bc0455fb0d2c3
{-# INLINABLE _y #-}

-- | Generator of affine BrainpoolP320T1 curve.
gA :: PA
gA = A _x _y
{-# INLINABLE gA #-}

-- | Generator of Jacobian BrainpoolP320T1 curve.
gJ :: PJ
gJ = J _x _y 1
{-# INLINABLE gJ #-}

-- | Generator of projective BrainpoolP320T1 curve.
gP :: PP
gP = P _x _y 1
{-# INLINABLE gP #-}
