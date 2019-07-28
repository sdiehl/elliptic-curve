module Curve.Weierstrass.BLS12381
  ( Curve(..)
  , Fq
  , Fr
  , Group(..)
  , P
  , Point(..)
  , WPoint
  , WCurve(..)
  , _a
  , _b
  , _g
  , _h
  , _q
  , _r
  , _x
  , _y
  ) where

import Protolude

import PrimeField (PrimeField)

import Curve (Curve(..))
import Curve.Weierstrass (Point(..), WCurve(..), WPoint)
import Group (Group(..))

-------------------------------------------------------------------------------
-- Types
-------------------------------------------------------------------------------

-- | BLS12381 curve.
data BLS12381

-- | Field of points of BLS12381 curve.
type Fq = PrimeField 0x1a0111ea397fe69a4b1ba7b6434bacd764774b84f38512bf6730d2a0f6b0f6241eabfffeb153ffffb9feffffffffaaab

-- | Field of coefficients of BLS12381 curve.
type Fr = PrimeField 0x73eda753299d7d483339d80809a1d80553bda402fffe5bfeffffffff00000001

-- | BLS12381 curve is a Weierstrass curve.
instance WCurve BLS12381 Fq where
  a_ = const _a
  {-# INLINE a_ #-}
  b_ = const _b
  {-# INLINE b_ #-}
  g_ = _g
  {-# INLINE g_ #-}
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

-- | Point of BLS12381 curve.
type P = WPoint BLS12381 Fq

-------------------------------------------------------------------------------
-- Parameters
-------------------------------------------------------------------------------

-- | Coefficient @A@ of BLS12381 curve.
_a :: Fq
_a = 0x0
{-# INLINE _a #-}

-- | Coefficient @B@ of BLS12381 curve.
_b :: Fq
_b = 0x4
{-# INLINE _b #-}

-- | Generator of BLS12381 curve.
_g :: P
_g = A _x _y
{-# INLINE _g #-}

-- | Cofactor of BLS12381 curve.
_h :: Integer
_h = 0x396c8c005555e1568c00aaab0000aaab
{-# INLINE _h #-}

-- | Characteristic of BLS12381 curve.
_q :: Integer
_q = 0x1a0111ea397fe69a4b1ba7b6434bacd764774b84f38512bf6730d2a0f6b0f6241eabfffeb153ffffb9feffffffffaaab
{-# INLINE _q #-}

-- | Order of BLS12381 curve.
_r :: Integer
_r = 0x73eda753299d7d483339d80809a1d80553bda402fffe5bfeffffffff00000001
{-# INLINE _r #-}

-- | Coordinate @X@ of BLS12381 curve.
_x :: Fq
_x = 0x17f1d3a73197d7942695638c4fa9ac0fc3688c4f9774b905a14e3a3f171bac586c55e83ff97a1aeffb3af00adb22c6bb
{-# INLINE _x #-}

-- | Coordinate @Y@ of BLS12381 curve.
_y :: Fq
_y = 0x8b3f481e3aaa0f1a09e30ed741d8ae4fcf5e095d5d00af600db18cb2c04b3edd03cc744a2888ae40caa232946c5e7e1
{-# INLINE _y #-}
