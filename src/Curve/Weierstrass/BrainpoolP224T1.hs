module Curve.Weierstrass.BrainpoolP224T1
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
-- BrainpoolP224T1 curve
-------------------------------------------------------------------------------

-- | BrainpoolP224T1 curve.
data BrainpoolP224T1

-- | Field of points of BrainpoolP224T1 curve.
type Fq = PrimeField 0xd7c134aa264366862a18302575d1d787b09f075797da89f57ec8c0ff

-- | Field of coefficients of BrainpoolP224T1 curve.
type Fr = PrimeField 0xd7c134aa264366862a18302575d0fb98d116bc4b6ddebca3a5a7939f

-- | BrainpoolP224T1 curve is a Weierstrass curve.
instance Curve 'Weierstrass c BrainpoolP224T1 Fq => WCurve c BrainpoolP224T1 Fq where
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

-- | Coefficient @A@ of BrainpoolP224T1 curve.
_a :: Fq
_a = 0xd7c134aa264366862a18302575d1d787b09f075797da89f57ec8c0fc
{-# INLINE _a #-}

-- | Coefficient @B@ of BrainpoolP224T1 curve.
_b :: Fq
_b = 0x4b337d934104cd7bef271bf60ced1ed20da14c08b3bb64f18a60888d
{-# INLINE _b #-}

-- | Cofactor of BrainpoolP224T1 curve.
_h :: Integer
_h = 0x1
{-# INLINE _h #-}

-- | Characteristic of BrainpoolP224T1 curve.
_q :: Integer
_q = 0xd7c134aa264366862a18302575d1d787b09f075797da89f57ec8c0ff
{-# INLINE _q #-}

-- | Order of BrainpoolP224T1 curve.
_r :: Integer
_r = 0xd7c134aa264366862a18302575d0fb98d116bc4b6ddebca3a5a7939f
{-# INLINE _r #-}

-------------------------------------------------------------------------------
-- Affine coordinates
-------------------------------------------------------------------------------

-- | Affine BrainpoolP224T1 point.
type AP = WAPoint BrainpoolP224T1 Fq

-- | Affine BrainpoolP224T1 curve is a Weierstrass affine curve.
instance WACurve BrainpoolP224T1 Fq where
  gA_ = gA
  {-# INLINE gA_ #-}
  xA_ = const xA
  {-# INLINE xA_ #-}
  yA_ = const yA
  {-# INLINE yA_ #-}

-- | Generator of affine BrainpoolP224T1 curve.
gA :: AP
gA = A xA yA
{-# INLINE gA #-}

-- | Coordinate @X@ of affine BrainpoolP224T1 curve.
xA :: Fq
xA = 0x6ab1e344ce25ff3896424e7ffe14762ecb49f8928ac0c76029b4d580
{-# INLINE xA #-}

-- | Coordinate @Y@ of affine BrainpoolP224T1 curve.
yA :: Fq
yA = 0x374e9f5143e568cd23f3f4d7c0d4b1e41c8cc0d1c6abd5f1a46db4c
{-# INLINE yA #-}
