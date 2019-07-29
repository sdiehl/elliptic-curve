module Curve.Weierstrass.SECP128R2
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
-- SECP128R2 curve
-------------------------------------------------------------------------------

-- | SECP128R2 curve.
data SECP128R2

-- | Field of points of SECP128R2 curve.
type Fq = PrimeField 0xfffffffdffffffffffffffffffffffff

-- | Field of coefficients of SECP128R2 curve.
type Fr = PrimeField 0x3fffffff7fffffffbe0024720613b5a3

-- | SECP128R2 curve is a Weierstrass curve.
instance Curve 'Weierstrass c SECP128R2 Fq => WCurve c SECP128R2 Fq where
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

-- | Coefficient @A@ of SECP128R2 curve.
_a :: Fq
_a = 0xd6031998d1b3bbfebf59cc9bbff9aee1
{-# INLINE _a #-}

-- | Coefficient @B@ of SECP128R2 curve.
_b :: Fq
_b = 0x5eeefca380d02919dc2c6558bb6d8a5d
{-# INLINE _b #-}

-- | Cofactor of SECP128R2 curve.
_h :: Integer
_h = 0x4
{-# INLINE _h #-}

-- | Characteristic of SECP128R2 curve.
_q :: Integer
_q = 0xfffffffdffffffffffffffffffffffff
{-# INLINE _q #-}

-- | Order of SECP128R2 curve.
_r :: Integer
_r = 0x3fffffff7fffffffbe0024720613b5a3
{-# INLINE _r #-}

-------------------------------------------------------------------------------
-- Affine coordinates
-------------------------------------------------------------------------------

-- | Affine SECP128R2 point.
type AP = WAPoint SECP128R2 Fq

-- | Affine SECP128R2 curve is a Weierstrass affine curve.
instance WACurve SECP128R2 Fq where
  g_ = _g
  {-# INLINE g_ #-}
  x_ = const _x
  {-# INLINE x_ #-}
  y_ = const _y
  {-# INLINE y_ #-}

-- | Generator of affine SECP128R2 curve.
_g :: AP
_g = A _x _y
{-# INLINE _g #-}

-- | Coordinate @X@ of affine SECP128R2 curve.
_x :: Fq
_x = 0x7b6aa5d85e572983e6fb32a7cdebc140
{-# INLINE _x #-}

-- | Coordinate @Y@ of affine SECP128R2 curve.
_y :: Fq
_y = 0x27b6916a894d3aee7106fe805fc34b44
{-# INLINE _y #-}
