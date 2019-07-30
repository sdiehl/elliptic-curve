module Curve.Weierstrass.BrainpoolP224R1
  ( AP
  , Curve(..)
  , Fq
  , Fr
  , Group(..)
  , Point(..)
  , WCurve(..)
  , WPoint
  , WACurve(..)
  , WAPoint
  , _a
  , _b
  , _h
  , _q
  , _r
  , gA
  , xA
  , yA
  ) where

import Protolude

import PrimeField (PrimeField)

import Curve (Curve(..), Form(..))
import Curve.Weierstrass (Point(..), WCurve(..), WPoint, WACurve(..), WAPoint)
import Group (Group(..))

-------------------------------------------------------------------------------
-- BrainpoolP224R1 curve
-------------------------------------------------------------------------------

-- | BrainpoolP224R1 curve.
data BrainpoolP224R1

-- | Field of points of BrainpoolP224R1 curve.
type Fq = PrimeField 0xd7c134aa264366862a18302575d1d787b09f075797da89f57ec8c0ff

-- | Field of coefficients of BrainpoolP224R1 curve.
type Fr = PrimeField 0xd7c134aa264366862a18302575d0fb98d116bc4b6ddebca3a5a7939f

-- | BrainpoolP224R1 curve is a Weierstrass curve.
instance Curve 'Weierstrass c BrainpoolP224R1 Fq => WCurve c BrainpoolP224R1 Fq where
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

-- | Coefficient @A@ of BrainpoolP224R1 curve.
_a :: Fq
_a = 0x68a5e62ca9ce6c1c299803a6c1530b514e182ad8b0042a59cad29f43
{-# INLINE _a #-}

-- | Coefficient @B@ of BrainpoolP224R1 curve.
_b :: Fq
_b = 0x2580f63ccfe44138870713b1a92369e33e2135d266dbb372386c400b
{-# INLINE _b #-}

-- | Cofactor of BrainpoolP224R1 curve.
_h :: Integer
_h = 0x1
{-# INLINE _h #-}

-- | Characteristic of BrainpoolP224R1 curve.
_q :: Integer
_q = 0xd7c134aa264366862a18302575d1d787b09f075797da89f57ec8c0ff
{-# INLINE _q #-}

-- | Order of BrainpoolP224R1 curve.
_r :: Integer
_r = 0xd7c134aa264366862a18302575d0fb98d116bc4b6ddebca3a5a7939f
{-# INLINE _r #-}

-------------------------------------------------------------------------------
-- Affine coordinates
-------------------------------------------------------------------------------

-- | Affine BrainpoolP224R1 point.
type AP = WAPoint BrainpoolP224R1 Fq

-- | Affine BrainpoolP224R1 curve is a Weierstrass affine curve.
instance WACurve BrainpoolP224R1 Fq where
  gA_ = gA
  {-# INLINE gA_ #-}
  xA_ = const xA
  {-# INLINE xA_ #-}
  yA_ = const yA
  {-# INLINE yA_ #-}

-- | Generator of affine BrainpoolP224R1 curve.
gA :: AP
gA = A xA yA
{-# INLINE gA #-}

-- | Coordinate @X@ of affine BrainpoolP224R1 curve.
xA :: Fq
xA = 0xd9029ad2c7e5cf4340823b2a87dc68c9e4ce3174c1e6efdee12c07d
{-# INLINE xA #-}

-- | Coordinate @Y@ of affine BrainpoolP224R1 curve.
yA :: Fq
yA = 0x58aa56f772c0726f24c6b89e4ecdac24354b9e99caa3f6d3761402cd
{-# INLINE yA #-}
