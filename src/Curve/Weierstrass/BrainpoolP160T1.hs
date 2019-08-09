module Curve.Weierstrass.BrainpoolP160T1
  ( Curve(..)
  , Fq
  , Fr
  , Group(..)
  , PA
  , PJ
  , PP
  , Point(..)
  , WCurve(..)
  , WPoint
  , WACurve(..)
  , WAPoint
  , WJCurve(..)
  , WJPoint
  , WPCurve(..)
  , WPPoint
  , _a
  , _b
  , _h
  , _q
  , _r
  , _x
  , _y
  , gA
  , gJ
  , gP
  ) where

import Protolude

import PrimeField

import Curve.Weierstrass

-------------------------------------------------------------------------------
-- Types
-------------------------------------------------------------------------------

-- | BrainpoolP160T1 curve.
data BrainpoolP160T1

-- | Field of points of BrainpoolP160T1 curve.
type Fq = PrimeField 0xe95e4a5f737059dc60dfc7ad95b3d8139515620f

-- | Field of coefficients of BrainpoolP160T1 curve.
type Fr = PrimeField 0xe95e4a5f737059dc60df5991d45029409e60fc09

-- | BrainpoolP160T1 curve is a Weierstrass curve.
instance Curve 'Weierstrass c BrainpoolP160T1 Fq Fr => WCurve c BrainpoolP160T1 Fq Fr where
  a_ = const _a
  {-# INLINE a_ #-}
  b_ = const _b
  {-# INLINE b_ #-}
  h_ = const _h
  {-# INLINE h_ #-}
  q_ = const _q
  {-# INLINE q_ #-}
  r_ = const _r
  {-# INLINE r_ #-}
  x_ = const _x
  {-# INLINE x_ #-}
  y_ = const _y
  {-# INLINE y_ #-}

-- | Affine BrainpoolP160T1 curve point.
type PA = WAPoint BrainpoolP160T1 Fq Fr

-- | Affine BrainpoolP160T1 curve is a Weierstrass affine curve.
instance WACurve BrainpoolP160T1 Fq Fr where
  gA_ = gA
  {-# INLINE gA_ #-}

-- | Jacobian BrainpoolP160T1 point.
type PJ = WJPoint BrainpoolP160T1 Fq Fr

-- | Jacobian BrainpoolP160T1 curve is a Weierstrass Jacobian curve.
instance WJCurve BrainpoolP160T1 Fq Fr where
  gJ_ = gJ
  {-# INLINE gJ_ #-}

-- | Projective BrainpoolP160T1 point.
type PP = WPPoint BrainpoolP160T1 Fq Fr

-- | Projective BrainpoolP160T1 curve is a Weierstrass projective curve.
instance WPCurve BrainpoolP160T1 Fq Fr where
  gP_ = gP
  {-# INLINE gP_ #-}

-------------------------------------------------------------------------------
-- Parameters
-------------------------------------------------------------------------------

-- | Coefficient @A@ of BrainpoolP160T1 curve.
_a :: Fq
_a = 0xe95e4a5f737059dc60dfc7ad95b3d8139515620c
{-# INLINE _a #-}

-- | Coefficient @B@ of BrainpoolP160T1 curve.
_b :: Fq
_b = 0x7a556b6dae535b7b51ed2c4d7daa7a0b5c55f380
{-# INLINE _b #-}

-- | Cofactor of BrainpoolP160T1 curve.
_h :: Integer
_h = 0x1
{-# INLINE _h #-}

-- | Characteristic of BrainpoolP160T1 curve.
_q :: Integer
_q = 0xe95e4a5f737059dc60dfc7ad95b3d8139515620f
{-# INLINE _q #-}

-- | Order of BrainpoolP160T1 curve.
_r :: Integer
_r = 0xe95e4a5f737059dc60df5991d45029409e60fc09
{-# INLINE _r #-}

-- | Coordinate @X@ of BrainpoolP160T1 curve.
_x :: Fq
_x = 0xb199b13b9b34efc1397e64baeb05acc265ff2378
{-# INLINE _x #-}

-- | Coordinate @Y@ of BrainpoolP160T1 curve.
_y :: Fq
_y = 0xadd6718b7c7c1961f0991b842443772152c9e0ad
{-# INLINE _y #-}

-- | Generator of affine BrainpoolP160T1 curve.
gA :: PA
gA = A _x _y
{-# INLINE gA #-}

-- | Generator of Jacobian BrainpoolP160T1 curve.
gJ :: PJ
gJ = J _x _y 1
{-# INLINE gJ #-}

-- | Generator of projective BrainpoolP160T1 curve.
gP :: PP
gP = P _x _y 1
{-# INLINE gP #-}
