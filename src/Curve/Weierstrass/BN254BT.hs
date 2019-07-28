module Curve.Weierstrass.BN254BT
  ( Curve(..)
  , Fp2
  , Group(..)
  , P
  , Point(..)
  , WPoint
  , WCurve(..)
  , _a
  , _b
  , _g
  , _h
  , _n
  , _p
  , _x
  , _y
  ) where

import Protolude

import ExtensionField

import Curve (Curve(..))
import Curve.Weierstrass (Point(..), WCurve(..), WPoint)
import Group (Group(..))
import Curve.Weierstrass.BN254B (Fp)

-------------------------------------------------------------------------------
-- Types
-------------------------------------------------------------------------------

-- | BN254BT curve.
data BN254BT

-- | Field of BN254BT curve.
data PolynomialU
instance IrreducibleMonic Fp PolynomialU where
  split _ = x * x + 1
type Fp2 = ExtensionField Fp PolynomialU

-- | BN254BT curve is a Weierstrass curve.
instance WCurve BN254BT Fp2 where
  a_ = const _a
  {-# INLINE a_ #-}
  b_ = const _b
  {-# INLINE b_ #-}
  g_ = _g
  {-# INLINE g_ #-}
  h_ = const _h
  {-# INLINE h_ #-}
  n_ = const _n
  {-# INLINE n_ #-}
  p_ = const _p
  {-# INLINE p_ #-}
  x_ = const _x
  {-# INLINE x_ #-}
  y_ = const _y
  {-# INLINE y_ #-}

-- | Point of BN254BT curve.
type P = WPoint BN254BT Fp2

-------------------------------------------------------------------------------
-- Parameters
-------------------------------------------------------------------------------

-- | Coefficient @A@ of BN254BT curve.
_a :: Fp2
_a = fromList [
              ]
{-# INLINE _a #-}

-- | Coefficient @B@ of BN254BT curve.
_b :: Fp2
_b = fromList [ 0x1
              , 0x2523648240000001ba344d80000000086121000000000013a700000000000012
              ]
{-# INLINE _b #-}

-- | Generator of BN254BT curve.
_g :: P
_g = A _x _y
{-# INLINE _g #-}

-- | Cofactor of BN254BT curve.
_h :: Integer
_h = 0x2523648240000001ba344d8000000008c2a2800000000016ad00000000000019
{-# INLINE _h #-}

-- | Order of BN254BT curve.
_n :: Integer
_n = 0x2523648240000001ba344d8000000007ff9f800000000010a10000000000000d
{-# INLINE _n #-}

-- | Characteristic of BN254BT curve.
_p :: Integer
_p = 0x2523648240000001ba344d80000000086121000000000013a700000000000013
{-# INLINE _p #-}

-- | Coordinate @X@ of BN254BT curve.
_x :: Fp2
_x = fromList [ 0x61a10bb519eb62feb8d8c7e8c61edb6a4648bbb4898bf0d91ee4224c803fb2b
              , 0x516aaf9ba737833310aa78c5982aa5b1f4d746bae3784b70d8c34c1e7d54cf3
              ]
{-# INLINE _x #-}

-- | Coordinate @Y@ of BN254BT curve.
_y :: Fp2
_y = fromList [ 0x21897a06baf93439a90e096698c822329bd0ae6bdbe09bd19f0e07891cd2b9a
              , 0xebb2b0e7c8b15268f6d4456f5f38d37b09006ffd739c9578a2d1aec6b3ace9b
              ]
{-# INLINE _y #-}
