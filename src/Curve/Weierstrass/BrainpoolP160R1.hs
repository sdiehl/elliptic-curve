module Curve.Weierstrass.BrainpoolP160R1
  ( module Curve.Weierstrass
  , module Curve.Weierstrass.BrainpoolP160R1
  , Point(..)
  ) where

import Protolude

import Data.Field.Galois

import Curve.Weierstrass

-------------------------------------------------------------------------------
-- Types
-------------------------------------------------------------------------------

-- | BrainpoolP160R1 curve.
data BrainpoolP160R1

-- | Field of points of BrainpoolP160R1 curve.
type Fq = Prime 0xe95e4a5f737059dc60dfc7ad95b3d8139515620f

-- | Field of coefficients of BrainpoolP160R1 curve.
type Fr = Prime 0xe95e4a5f737059dc60df5991d45029409e60fc09

-- | BrainpoolP160R1 curve is a Weierstrass curve.
instance Curve 'Weierstrass c BrainpoolP160R1 Fq Fr => WCurve c BrainpoolP160R1 Fq Fr where
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

-- | Affine BrainpoolP160R1 curve point.
type PA = WAPoint BrainpoolP160R1 Fq Fr

-- | Affine BrainpoolP160R1 curve is a Weierstrass affine curve.
instance WACurve BrainpoolP160R1 Fq Fr where
  gA_ = gA
  {-# INLINABLE gA_ #-}

-- | Jacobian BrainpoolP160R1 point.
type PJ = WJPoint BrainpoolP160R1 Fq Fr

-- | Jacobian BrainpoolP160R1 curve is a Weierstrass Jacobian curve.
instance WJCurve BrainpoolP160R1 Fq Fr where
  gJ_ = gJ
  {-# INLINABLE gJ_ #-}

-- | Projective BrainpoolP160R1 point.
type PP = WPPoint BrainpoolP160R1 Fq Fr

-- | Projective BrainpoolP160R1 curve is a Weierstrass projective curve.
instance WPCurve BrainpoolP160R1 Fq Fr where
  gP_ = gP
  {-# INLINABLE gP_ #-}

-------------------------------------------------------------------------------
-- Parameters
-------------------------------------------------------------------------------

-- | Coefficient @A@ of BrainpoolP160R1 curve.
_a :: Fq
_a = 0x340e7be2a280eb74e2be61bada745d97e8f7c300
{-# INLINABLE _a #-}

-- | Coefficient @B@ of BrainpoolP160R1 curve.
_b :: Fq
_b = 0x1e589a8595423412134faa2dbdec95c8d8675e58
{-# INLINABLE _b #-}

-- | Cofactor of BrainpoolP160R1 curve.
_h :: Integer
_h = 0x1
{-# INLINABLE _h #-}

-- | Characteristic of BrainpoolP160R1 curve.
_q :: Integer
_q = 0xe95e4a5f737059dc60dfc7ad95b3d8139515620f
{-# INLINABLE _q #-}

-- | Order of BrainpoolP160R1 curve.
_r :: Integer
_r = 0xe95e4a5f737059dc60df5991d45029409e60fc09
{-# INLINABLE _r #-}

-- | Coordinate @X@ of BrainpoolP160R1 curve.
_x :: Fq
_x = 0xbed5af16ea3f6a4f62938c4631eb5af7bdbcdbc3
{-# INLINABLE _x #-}

-- | Coordinate @Y@ of BrainpoolP160R1 curve.
_y :: Fq
_y = 0x1667cb477a1a8ec338f94741669c976316da6321
{-# INLINABLE _y #-}

-- | Generator of affine BrainpoolP160R1 curve.
gA :: PA
gA = A _x _y
{-# INLINABLE gA #-}

-- | Generator of Jacobian BrainpoolP160R1 curve.
gJ :: PJ
gJ = J _x _y 1
{-# INLINABLE gJ #-}

-- | Generator of projective BrainpoolP160R1 curve.
gP :: PP
gP = P _x _y 1
{-# INLINABLE gP #-}
