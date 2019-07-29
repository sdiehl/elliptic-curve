module Curve.Weierstrass.SECP160K1
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
  , _g
  , _h
  , _q
  , _r
  , _x
  , _y
  ) where

import Protolude

import PrimeField (PrimeField)

import Curve (Curve(..), Form(..))
import Curve.Weierstrass (Point(..), WCurve(..), WPoint, WACurve(..), WAPoint)
import Group (Group(..))

-------------------------------------------------------------------------------
-- SECP160K1 curve
-------------------------------------------------------------------------------

-- | SECP160K1 curve.
data SECP160K1

-- | Field of points of SECP160K1 curve.
type Fq = PrimeField 0xfffffffffffffffffffffffffffffffeffffac73

-- | Field of coefficients of SECP160K1 curve.
type Fr = PrimeField 0x100000000000000000001b8fa16dfab9aca16b6b3

-- | SECP160K1 curve is a Weierstrass curve.
instance Curve 'Weierstrass c SECP160K1 Fq => WCurve c SECP160K1 Fq where
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

-- | Coefficient @A@ of SECP160K1 curve.
_a :: Fq
_a = 0x0
{-# INLINE _a #-}

-- | Coefficient @B@ of SECP160K1 curve.
_b :: Fq
_b = 0x7
{-# INLINE _b #-}

-- | Cofactor of SECP160K1 curve.
_h :: Integer
_h = 0x1
{-# INLINE _h #-}

-- | Characteristic of SECP160K1 curve.
_q :: Integer
_q = 0xfffffffffffffffffffffffffffffffeffffac73
{-# INLINE _q #-}

-- | Order of SECP160K1 curve.
_r :: Integer
_r = 0x100000000000000000001b8fa16dfab9aca16b6b3
{-# INLINE _r #-}

-------------------------------------------------------------------------------
-- Affine coordinates
-------------------------------------------------------------------------------

-- | Affine SECP160K1 point.
type AP = WAPoint SECP160K1 Fq

-- | Affine SECP160K1 curve is a Weierstrass affine curve.
instance WACurve SECP160K1 Fq where
  g_ = _g
  {-# INLINE g_ #-}
  x_ = const _x
  {-# INLINE x_ #-}
  y_ = const _y
  {-# INLINE y_ #-}

-- | Generator of affine SECP160K1 curve.
_g :: AP
_g = A _x _y
{-# INLINE _g #-}

-- | Coordinate @X@ of affine SECP160K1 curve.
_x :: Fq
_x = 0x3b4c382ce37aa192a4019e763036f4f5dd4d7ebb
{-# INLINE _x #-}

-- | Coordinate @Y@ of affine SECP160K1 curve.
_y :: Fq
_y = 0x938cf935318fdced6bc28286531733c3f03c4fee
{-# INLINE _y #-}
