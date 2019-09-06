module Data.Curve.Weierstrass.BrainpoolP224R1
  ( module Data.Curve.Weierstrass
  -- * BrainpoolP224R1 curve
  , module Data.Curve.Weierstrass.BrainpoolP224R1
  ) where

import Protolude

import Data.Field.Galois
import GHC.Natural (Natural)

import Data.Curve.Weierstrass
import Data.Curve.Weierstrass.Base (WCurve(..), WACurve(..), WJCurve(..), WPCurve(..))

-------------------------------------------------------------------------------
-- Types
-------------------------------------------------------------------------------

-- | BrainpoolP224R1 curve.
data BrainpoolP224R1

-- | Field of points of BrainpoolP224R1 curve.
type Fq = Prime Q
type Q = 0xd7c134aa264366862a18302575d1d787b09f075797da89f57ec8c0ff

-- | Field of coefficients of BrainpoolP224R1 curve.
type Fr = Prime R
type R = 0xd7c134aa264366862a18302575d0fb98d116bc4b6ddebca3a5a7939f

-- BrainpoolP224R1 curve is a Weierstrass curve.
instance Curve 'Weierstrass c BrainpoolP224R1 Fq Fr => WCurve c BrainpoolP224R1 Fq Fr where
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

-- | Affine BrainpoolP224R1 curve point.
type PA = WAPoint BrainpoolP224R1 Fq Fr

-- Affine BrainpoolP224R1 curve is a Weierstrass affine curve.
instance WACurve BrainpoolP224R1 Fq Fr where
  gA_ = gA
  {-# INLINABLE gA_ #-}

-- | Jacobian BrainpoolP224R1 point.
type PJ = WJPoint BrainpoolP224R1 Fq Fr

-- Jacobian BrainpoolP224R1 curve is a Weierstrass Jacobian curve.
instance WJCurve BrainpoolP224R1 Fq Fr where
  gJ_ = gJ
  {-# INLINABLE gJ_ #-}

-- | Projective BrainpoolP224R1 point.
type PP = WPPoint BrainpoolP224R1 Fq Fr

-- Projective BrainpoolP224R1 curve is a Weierstrass projective curve.
instance WPCurve BrainpoolP224R1 Fq Fr where
  gP_ = gP
  {-# INLINABLE gP_ #-}

-------------------------------------------------------------------------------
-- Parameters
-------------------------------------------------------------------------------

-- | Coefficient @A@ of BrainpoolP224R1 curve.
_a :: Fq
_a = 0x68a5e62ca9ce6c1c299803a6c1530b514e182ad8b0042a59cad29f43
{-# INLINABLE _a #-}

-- | Coefficient @B@ of BrainpoolP224R1 curve.
_b :: Fq
_b = 0x2580f63ccfe44138870713b1a92369e33e2135d266dbb372386c400b
{-# INLINABLE _b #-}

-- | Cofactor of BrainpoolP224R1 curve.
_h :: Natural
_h = 0x1
{-# INLINABLE _h #-}

-- | Characteristic of BrainpoolP224R1 curve.
_q :: Natural
_q = 0xd7c134aa264366862a18302575d1d787b09f075797da89f57ec8c0ff
{-# INLINABLE _q #-}

-- | Order of BrainpoolP224R1 curve.
_r :: Natural
_r = 0xd7c134aa264366862a18302575d0fb98d116bc4b6ddebca3a5a7939f
{-# INLINABLE _r #-}

-- | Coordinate @X@ of BrainpoolP224R1 curve.
_x :: Fq
_x = 0xd9029ad2c7e5cf4340823b2a87dc68c9e4ce3174c1e6efdee12c07d
{-# INLINABLE _x #-}

-- | Coordinate @Y@ of BrainpoolP224R1 curve.
_y :: Fq
_y = 0x58aa56f772c0726f24c6b89e4ecdac24354b9e99caa3f6d3761402cd
{-# INLINABLE _y #-}

-- | Generator of affine BrainpoolP224R1 curve.
gA :: PA
gA = A _x _y
{-# INLINABLE gA #-}

-- | Generator of Jacobian BrainpoolP224R1 curve.
gJ :: PJ
gJ = J _x _y 1
{-# INLINABLE gJ #-}

-- | Generator of projective BrainpoolP224R1 curve.
gP :: PP
gP = P _x _y 1
{-# INLINABLE gP #-}
