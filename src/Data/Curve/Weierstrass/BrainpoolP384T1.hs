module Data.Curve.Weierstrass.BrainpoolP384T1
  ( module Data.Curve.Weierstrass
  , Point(..)
  -- * BrainpoolP384T1 curve
  , module Data.Curve.Weierstrass.BrainpoolP384T1
  ) where

import Protolude

import Data.Field.Galois
import GHC.Natural (Natural)

import Data.Curve.Weierstrass

-------------------------------------------------------------------------------
-- Types
-------------------------------------------------------------------------------

-- | BrainpoolP384T1 curve.
data BrainpoolP384T1

-- | Field of points of BrainpoolP384T1 curve.
type Fq = Prime Q
type Q = 0x8cb91e82a3386d280f5d6f7e50e641df152f7109ed5456b412b1da197fb71123acd3a729901d1a71874700133107ec53

-- | Field of coefficients of BrainpoolP384T1 curve.
type Fr = Prime R
type R = 0x8cb91e82a3386d280f5d6f7e50e641df152f7109ed5456b31f166e6cac0425a7cf3ab6af6b7fc3103b883202e9046565

-- BrainpoolP384T1 curve is a Weierstrass curve.
instance Curve 'Weierstrass c BrainpoolP384T1 Fq Fr => WCurve c BrainpoolP384T1 Fq Fr where
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

-- | Affine BrainpoolP384T1 curve point.
type PA = WAPoint BrainpoolP384T1 Fq Fr

-- Affine BrainpoolP384T1 curve is a Weierstrass affine curve.
instance WACurve BrainpoolP384T1 Fq Fr where
  gA_ = gA
  {-# INLINABLE gA_ #-}

-- | Jacobian BrainpoolP384T1 point.
type PJ = WJPoint BrainpoolP384T1 Fq Fr

-- Jacobian BrainpoolP384T1 curve is a Weierstrass Jacobian curve.
instance WJCurve BrainpoolP384T1 Fq Fr where
  gJ_ = gJ
  {-# INLINABLE gJ_ #-}

-- | Projective BrainpoolP384T1 point.
type PP = WPPoint BrainpoolP384T1 Fq Fr

-- Projective BrainpoolP384T1 curve is a Weierstrass projective curve.
instance WPCurve BrainpoolP384T1 Fq Fr where
  gP_ = gP
  {-# INLINABLE gP_ #-}

-------------------------------------------------------------------------------
-- Parameters
-------------------------------------------------------------------------------

-- | Coefficient @A@ of BrainpoolP384T1 curve.
_a :: Fq
_a = 0x8cb91e82a3386d280f5d6f7e50e641df152f7109ed5456b412b1da197fb71123acd3a729901d1a71874700133107ec50
{-# INLINABLE _a #-}

-- | Coefficient @B@ of BrainpoolP384T1 curve.
_b :: Fq
_b = 0x7f519eada7bda81bd826dba647910f8c4b9346ed8ccdc64e4b1abd11756dce1d2074aa263b88805ced70355a33b471ee
{-# INLINABLE _b #-}

-- | Cofactor of BrainpoolP384T1 curve.
_h :: Natural
_h = 0x1
{-# INLINABLE _h #-}

-- | Characteristic of BrainpoolP384T1 curve.
_q :: Natural
_q = 0x8cb91e82a3386d280f5d6f7e50e641df152f7109ed5456b412b1da197fb71123acd3a729901d1a71874700133107ec53
{-# INLINABLE _q #-}

-- | Order of BrainpoolP384T1 curve.
_r :: Natural
_r = 0x8cb91e82a3386d280f5d6f7e50e641df152f7109ed5456b31f166e6cac0425a7cf3ab6af6b7fc3103b883202e9046565
{-# INLINABLE _r #-}

-- | Coordinate @X@ of BrainpoolP384T1 curve.
_x :: Fq
_x = 0x18de98b02db9a306f2afcd7235f72a819b80ab12ebd653172476fecd462aabffc4ff191b946a5f54d8d0aa2f418808cc
{-# INLINABLE _x #-}

-- | Coordinate @Y@ of BrainpoolP384T1 curve.
_y :: Fq
_y = 0x25ab056962d30651a114afd2755ad336747f93475b7a1fca3b88f2b6a208ccfe469408584dc2b2912675bf5b9e582928
{-# INLINABLE _y #-}

-- | Generator of affine BrainpoolP384T1 curve.
gA :: PA
gA = A _x _y
{-# INLINABLE gA #-}

-- | Generator of Jacobian BrainpoolP384T1 curve.
gJ :: PJ
gJ = J _x _y 1
{-# INLINABLE gJ #-}

-- | Generator of projective BrainpoolP384T1 curve.
gP :: PP
gP = P _x _y 1
{-# INLINABLE gP #-}
