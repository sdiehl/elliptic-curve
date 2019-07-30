module Curve.Weierstrass.SECP112R2
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
-- SECP112R2 curve
-------------------------------------------------------------------------------

-- | SECP112R2 curve.
data SECP112R2

-- | Field of points of SECP112R2 curve.
type Fq = PrimeField 0xdb7c2abf62e35e668076bead208b

-- | Field of coefficients of SECP112R2 curve.
type Fr = PrimeField 0x36df0aafd8b8d7597ca10520d04b

-- | SECP112R2 curve is a Weierstrass curve.
instance Curve 'Weierstrass c SECP112R2 Fq => WCurve c SECP112R2 Fq where
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

-- | Coefficient @A@ of SECP112R2 curve.
_a :: Fq
_a = 0x6127c24c05f38a0aaaf65c0ef02c
{-# INLINE _a #-}

-- | Coefficient @B@ of SECP112R2 curve.
_b :: Fq
_b = 0x51def1815db5ed74fcc34c85d709
{-# INLINE _b #-}

-- | Cofactor of SECP112R2 curve.
_h :: Integer
_h = 0x4
{-# INLINE _h #-}

-- | Characteristic of SECP112R2 curve.
_q :: Integer
_q = 0xdb7c2abf62e35e668076bead208b
{-# INLINE _q #-}

-- | Order of SECP112R2 curve.
_r :: Integer
_r = 0x36df0aafd8b8d7597ca10520d04b
{-# INLINE _r #-}

-------------------------------------------------------------------------------
-- Affine coordinates
-------------------------------------------------------------------------------

-- | Affine SECP112R2 point.
type AP = WAPoint SECP112R2 Fq

-- | Affine SECP112R2 curve is a Weierstrass affine curve.
instance WACurve SECP112R2 Fq where
  gA_ = gA
  {-# INLINE gA_ #-}
  xA_ = const xA
  {-# INLINE xA_ #-}
  yA_ = const yA
  {-# INLINE yA_ #-}

-- | Generator of affine SECP112R2 curve.
gA :: AP
gA = A xA yA
{-# INLINE gA #-}

-- | Coordinate @X@ of affine SECP112R2 curve.
xA :: Fq
xA = 0x4ba30ab5e892b4e1649dd0928643
{-# INLINE xA #-}

-- | Coordinate @Y@ of affine SECP112R2 curve.
yA :: Fq
yA = 0xadcd46f5882e3747def36e956e97
{-# INLINE yA #-}
