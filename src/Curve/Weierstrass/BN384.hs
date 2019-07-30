module Curve.Weierstrass.BN384
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
-- BN384 curve
-------------------------------------------------------------------------------

-- | BN384 curve.
data BN384

-- | Field of points of BN384 curve.
type Fq = PrimeField 0xfffffffffffffffffff2a96823d5920d2a127e3f6fbca024c8fbe29531892c79534f9d306328261550a7cabd7cccd10b

-- | Field of coefficients of BN384 curve.
type Fr = PrimeField 0xfffffffffffffffffff2a96823d5920d2a127e3f6fbca023c8fbe29531892c795356487d8ac63e4f4db17384341a5775

-- | BN384 curve is a Weierstrass curve.
instance Curve 'Weierstrass c BN384 Fq => WCurve c BN384 Fq where
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

-- | Coefficient @A@ of BN384 curve.
_a :: Fq
_a = 0x0
{-# INLINE _a #-}

-- | Coefficient @B@ of BN384 curve.
_b :: Fq
_b = 0x3
{-# INLINE _b #-}

-- | Cofactor of BN384 curve.
_h :: Integer
_h = 0x1
{-# INLINE _h #-}

-- | Characteristic of BN384 curve.
_q :: Integer
_q = 0xfffffffffffffffffff2a96823d5920d2a127e3f6fbca024c8fbe29531892c79534f9d306328261550a7cabd7cccd10b
{-# INLINE _q #-}

-- | Order of BN384 curve.
_r :: Integer
_r = 0xfffffffffffffffffff2a96823d5920d2a127e3f6fbca023c8fbe29531892c795356487d8ac63e4f4db17384341a5775
{-# INLINE _r #-}

-------------------------------------------------------------------------------
-- Affine coordinates
-------------------------------------------------------------------------------

-- | Affine BN384 point.
type AP = WAPoint BN384 Fq

-- | Affine BN384 curve is a Weierstrass affine curve.
instance WACurve BN384 Fq where
  gA_ = gA
  {-# INLINE gA_ #-}
  xA_ = const xA
  {-# INLINE xA_ #-}
  yA_ = const yA
  {-# INLINE yA_ #-}

-- | Generator of affine BN384 curve.
gA :: AP
gA = A xA yA
{-# INLINE gA #-}

-- | Coordinate @X@ of affine BN384 curve.
xA :: Fq
xA = 0x1
{-# INLINE xA #-}

-- | Coordinate @Y@ of affine BN384 curve.
yA :: Fq
yA = 0x2
{-# INLINE yA #-}
