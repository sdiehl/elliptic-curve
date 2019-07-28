module Curve.Montgomery.Curve448
  ( Curve(..)
  , Fq
  , Fr
  , Group(..)
  , MPoint
  , MCurve(..)
  , P
  , Point(..)
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
import Curve.Montgomery (MCurve(..), MPoint, Point(..))
import Group (Group(..))

-------------------------------------------------------------------------------
-- Types
-------------------------------------------------------------------------------

-- | Curve448 curve.
data Curve448

-- | Field of points of Curve448 curve.
type Fq = PrimeField 0xfffffffffffffffffffffffffffffffffffffffffffffffffffffffeffffffffffffffffffffffffffffffffffffffffffffffffffffffff

-- | Field of coefficients of Curve448 curve.
type Fr = PrimeField 0x3fffffffffffffffffffffffffffffffffffffffffffffffffffffff7cca23e9c44edb49aed63690216cc2728dc58f552378c292ab5844f3

-- | Curve448 curve is a Montgomery curve.
instance MCurve Curve448 Fq where
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

-- | Point of Curve448 curve.
type P = MPoint Curve448 Fq

-------------------------------------------------------------------------------
-- Parameters
-------------------------------------------------------------------------------

-- | Coefficient @A@ of Curve448 curve.
_a :: Fq
_a = 0x262a6
{-# INLINE _a #-}

-- | Coefficient @B@ of Curve448 curve.
_b :: Fq
_b = 0x1
{-# INLINE _b #-}

-- | Generator of Curve448 curve.
_g :: P
_g = A _x _y
{-# INLINE _g #-}

-- | Cofactor of Curve448 curve.
_h :: Integer
_h = 0x4
{-# INLINE _h #-}

-- | Characteristic of Curve448 curve.
_q :: Integer
_q = 0xfffffffffffffffffffffffffffffffffffffffffffffffffffffffeffffffffffffffffffffffffffffffffffffffffffffffffffffffff
{-# INLINE _q #-}

-- | Order of Curve448 curve.
_r :: Integer
_r = 0x3fffffffffffffffffffffffffffffffffffffffffffffffffffffff7cca23e9c44edb49aed63690216cc2728dc58f552378c292ab5844f3
{-# INLINE _r #-}

-- | Coordinate @X@ of Curve448 curve.
_x :: Fq
_x = 0x5
{-# INLINE _x #-}

-- | Coordinate @Y@ of Curve448 curve.
_y :: Fq
_y = 0x7d235d1295f5b1f66c98ab6e58326fcecbae5d34f55545d060f75dc28df3f6edb8027e2346430d211312c4b150677af76fd7223d457b5b1a
{-# INLINE _y #-}
