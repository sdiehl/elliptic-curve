module Curve.Weierstrass.BN254BT
  ( AP
  , Curve(..)
  , Fq2
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

import ExtensionField
import PrimeField (PrimeField)

import Curve (Curve(..), Form(..))
import Curve.Weierstrass (Point(..), WCurve(..), WPoint, WACurve(..), WAPoint)
import Curve.Weierstrass.BN254B (Fq)
import Group (Group(..))

-------------------------------------------------------------------------------
-- BN254BT curve
-------------------------------------------------------------------------------

-- | BN254BT curve.
data BN254BT

-- | Field of points of BN254BT curve.
data PolynomialU
instance IrreducibleMonic Fq PolynomialU where
  split _ = x * x + 1
type Fq2 = ExtensionField Fq PolynomialU

-- | Field of coefficients of BN254BT curve.
type Fr = PrimeField 0x2523648240000001ba344d8000000007ff9f800000000010a10000000000000d

-- | BN254BT curve is a Weierstrass curve.
instance Curve 'Weierstrass c BN254BT Fq2 => WCurve c BN254BT Fq2 where
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

-- | Coefficient @A@ of BN254BT curve.
_a :: Fq2
_a = fromList [
              ]
{-# INLINE _a #-}

-- | Coefficient @B@ of BN254BT curve.
_b :: Fq2
_b = fromList [ 0x1
              , 0x2523648240000001ba344d80000000086121000000000013a700000000000012
              ]
{-# INLINE _b #-}

-- | Cofactor of BN254BT curve.
_h :: Integer
_h = 0x2523648240000001ba344d8000000008c2a2800000000016ad00000000000019
{-# INLINE _h #-}

-- | Characteristic of BN254BT curve.
_q :: Integer
_q = 0x2523648240000001ba344d80000000086121000000000013a700000000000013
{-# INLINE _q #-}

-- | Order of BN254BT curve.
_r :: Integer
_r = 0x2523648240000001ba344d8000000007ff9f800000000010a10000000000000d
{-# INLINE _r #-}

-------------------------------------------------------------------------------
-- Affine coordinates
-------------------------------------------------------------------------------

-- | Affine BN254BT point.
type AP = WAPoint BN254BT Fq2

-- | Affine BN254BT curve is a Weierstrass affine curve.
instance WACurve BN254BT Fq2 where
  gA_ = gA
  {-# INLINE gA_ #-}
  xA_ = const xA
  {-# INLINE xA_ #-}
  yA_ = const yA
  {-# INLINE yA_ #-}

-- | Generator of affine BN254BT curve.
gA :: AP
gA = A xA yA
{-# INLINE gA #-}

-- | Coordinate @X@ of affine BN254BT curve.
xA :: Fq2
xA = fromList [ 0x61a10bb519eb62feb8d8c7e8c61edb6a4648bbb4898bf0d91ee4224c803fb2b
              , 0x516aaf9ba737833310aa78c5982aa5b1f4d746bae3784b70d8c34c1e7d54cf3
              ]
{-# INLINE xA #-}

-- | Coordinate @Y@ of affine BN254BT curve.
yA :: Fq2
yA = fromList [ 0x21897a06baf93439a90e096698c822329bd0ae6bdbe09bd19f0e07891cd2b9a
              , 0xebb2b0e7c8b15268f6d4456f5f38d37b09006ffd739c9578a2d1aec6b3ace9b
              ]
{-# INLINE yA #-}
