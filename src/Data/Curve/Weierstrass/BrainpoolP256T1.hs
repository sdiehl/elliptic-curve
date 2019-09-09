module Data.Curve.Weierstrass.BrainpoolP256T1
  ( module Data.Curve.Weierstrass
  -- * BrainpoolP256T1 curve
  , module Data.Curve.Weierstrass.BrainpoolP256T1
  ) where

import Protolude

import Data.Field.Galois
import GHC.Natural (Natural)

import Data.Curve.Weierstrass

-------------------------------------------------------------------------------
-- Types
-------------------------------------------------------------------------------

-- | BrainpoolP256T1 curve.
data BrainpoolP256T1

-- | Field of points of BrainpoolP256T1 curve.
type Fq = Prime Q
type Q = 0xa9fb57dba1eea9bc3e660a909d838d726e3bf623d52620282013481d1f6e5377

-- | Field of coefficients of BrainpoolP256T1 curve.
type Fr = Prime R
type R = 0xa9fb57dba1eea9bc3e660a909d838d718c397aa3b561a6f7901e0e82974856a7

-- BrainpoolP256T1 curve is a Weierstrass curve.
instance Curve 'Weierstrass c BrainpoolP256T1 Fq Fr => WCurve c BrainpoolP256T1 Fq Fr where
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

-- | Affine BrainpoolP256T1 curve point.
type PA = WAPoint BrainpoolP256T1 Fq Fr

-- Affine BrainpoolP256T1 curve is a Weierstrass affine curve.
instance WACurve BrainpoolP256T1 Fq Fr where
  gA_ = gA
  {-# INLINABLE gA_ #-}

-- | Jacobian BrainpoolP256T1 point.
type PJ = WJPoint BrainpoolP256T1 Fq Fr

-- Jacobian BrainpoolP256T1 curve is a Weierstrass Jacobian curve.
instance WJCurve BrainpoolP256T1 Fq Fr where
  gJ_ = gJ
  {-# INLINABLE gJ_ #-}

-- | Projective BrainpoolP256T1 point.
type PP = WPPoint BrainpoolP256T1 Fq Fr

-- Projective BrainpoolP256T1 curve is a Weierstrass projective curve.
instance WPCurve BrainpoolP256T1 Fq Fr where
  gP_ = gP
  {-# INLINABLE gP_ #-}

-------------------------------------------------------------------------------
-- Parameters
-------------------------------------------------------------------------------

-- | Coefficient @A@ of BrainpoolP256T1 curve.
_a :: Fq
_a = 0xa9fb57dba1eea9bc3e660a909d838d726e3bf623d52620282013481d1f6e5374
{-# INLINABLE _a #-}

-- | Coefficient @B@ of BrainpoolP256T1 curve.
_b :: Fq
_b = 0x662c61c430d84ea4fe66a7733d0b76b7bf93ebc4af2f49256ae58101fee92b04
{-# INLINABLE _b #-}

-- | Cofactor of BrainpoolP256T1 curve.
_h :: Natural
_h = 0x1
{-# INLINABLE _h #-}

-- | Characteristic of BrainpoolP256T1 curve.
_q :: Natural
_q = 0xa9fb57dba1eea9bc3e660a909d838d726e3bf623d52620282013481d1f6e5377
{-# INLINABLE _q #-}

-- | Order of BrainpoolP256T1 curve.
_r :: Natural
_r = 0xa9fb57dba1eea9bc3e660a909d838d718c397aa3b561a6f7901e0e82974856a7
{-# INLINABLE _r #-}

-- | Coordinate @X@ of BrainpoolP256T1 curve.
_x :: Fq
_x = 0xa3e8eb3cc1cfe7b7732213b23a656149afa142c47aafbc2b79a191562e1305f4
{-# INLINABLE _x #-}

-- | Coordinate @Y@ of BrainpoolP256T1 curve.
_y :: Fq
_y = 0x2d996c823439c56d7f7b22e14644417e69bcb6de39d027001dabe8f35b25c9be
{-# INLINABLE _y #-}

-- | Generator of affine BrainpoolP256T1 curve.
gA :: PA
gA = A _x _y
{-# INLINABLE gA #-}

-- | Generator of Jacobian BrainpoolP256T1 curve.
gJ :: PJ
gJ = J _x _y 1
{-# INLINABLE gJ #-}

-- | Generator of projective BrainpoolP256T1 curve.
gP :: PP
gP = P _x _y 1
{-# INLINABLE gP #-}
