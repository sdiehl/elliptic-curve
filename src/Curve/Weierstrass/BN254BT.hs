module Curve.Weierstrass.BN254BT
  ( Curve(..)
  , Fq2
  , Fr
  , Group(..)
  , PA
  , PP
  , WCurve(..)
  , WPoint
  , WACurve(..)
  , WAPoint
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
  , pattern A
  , pattern J
  , pattern P
  ) where

import Protolude

import ExtensionField
import PrimeField

import Curve.Weierstrass
import Curve.Weierstrass.BN254B (Fq)

-------------------------------------------------------------------------------
-- Types
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
  x_ = const _x
  {-# INLINE x_ #-}
  y_ = const _y
  {-# INLINE y_ #-}

-- | Affine BN254BT curve point.
type PA = WAPoint BN254BT Fq2

-- | Affine BN254BT curve is a Weierstrass affine curve.
instance WACurve BN254BT Fq2 where
  gA_ = gA
  {-# INLINE gA_ #-}

-- | Jacobian BN254BT point.
type PJ = WJPoint BN254BT Fq2

-- | Jacobian BN254BT curve is a Weierstrass Jacobian curve.
instance WJCurve BN254BT Fq2 where
  gJ_ = gJ
  {-# INLINE gJ_ #-}

-- | Projective BN254BT point.
type PP = WPPoint BN254BT Fq2

-- | Projective BN254BT curve is a Weierstrass projective curve.
instance WPCurve BN254BT Fq2 where
  gP_ = gP
  {-# INLINE gP_ #-}

-------------------------------------------------------------------------------
-- Parameters
-------------------------------------------------------------------------------

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

-- | Coordinate @X@ of BN254BT curve.
_x :: Fq2
_x = fromList [ 0x61a10bb519eb62feb8d8c7e8c61edb6a4648bbb4898bf0d91ee4224c803fb2b
              , 0x516aaf9ba737833310aa78c5982aa5b1f4d746bae3784b70d8c34c1e7d54cf3
              ]
{-# INLINE _x #-}

-- | Coordinate @Y@ of BN254BT curve.
_y :: Fq2
_y = fromList [ 0x21897a06baf93439a90e096698c822329bd0ae6bdbe09bd19f0e07891cd2b9a
              , 0xebb2b0e7c8b15268f6d4456f5f38d37b09006ffd739c9578a2d1aec6b3ace9b
              ]
{-# INLINE _y #-}

-- | Generator of affine BN254BT curve.
gA :: PA
gA = fromMaybe (panic "not well-defined.") (point _x _y)
{-# INLINE gA #-}

-- | Generator of Jacobian BN254BT curve.
gJ :: PJ
gJ = fromMaybe (panic "not well-defined.") (point _x _y)
{-# INLINE gJ #-}

-- | Generator of projective BN254BT curve.
gP :: PP
gP = fromMaybe (panic "not well-defined.") (point _x _y)
{-# INLINE gP #-}
