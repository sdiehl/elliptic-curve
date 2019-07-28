module Curve.Edwards.Ed448
  ( Curve(..)
  , EPoint
  , ECurve(..)
  , Fp
  , Group(..)
  , P
  , Point(..)
  , _a
  , _d
  , _g
  , _h
  , _n
  , _p
  , _x
  , _y
  ) where

import Protolude

import PrimeField (PrimeField)

import Curve (Curve(..))
import Curve.Edwards (ECurve(..), EPoint, Point(..))
import Group (Group(..))

-------------------------------------------------------------------------------
-- Types
-------------------------------------------------------------------------------

-- | Ed448 curve.
data Ed448

-- | Field of Ed448 curve.
type Fp = PrimeField 0xfffffffffffffffffffffffffffffffffffffffffffffffffffffffeffffffffffffffffffffffffffffffffffffffffffffffffffffffff

-- | Ed448 curve is an Edwards curve.
instance ECurve Ed448 Fp where
  a_ = const _a
  {-# INLINE a_ #-}
  d_ = const _d
  {-# INLINE d_ #-}
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

-- | Point of Ed448 curve.
type P = EPoint Ed448 Fp

-------------------------------------------------------------------------------
-- Parameters
-------------------------------------------------------------------------------

-- | Coefficient @A@ of Ed448 curve.
_a :: Fp
_a = 0x1
{-# INLINE _a #-}

-- | Coefficient @B@ of Ed448 curve.
_d :: Fp
_d = 0xfffffffffffffffffffffffffffffffffffffffffffffffffffffffeffffffffffffffffffffffffffffffffffffffffffffffffffff6756
{-# INLINE _d #-}

-- | Generator of Ed448 curve.
_g :: P
_g = A _x _y
{-# INLINE _g #-}

-- | Cofactor of Ed448 curve.
_h :: Integer
_h = 0x4
{-# INLINE _h #-}

-- | Order of Ed448 curve.
_n :: Integer
_n = 0x3fffffffffffffffffffffffffffffffffffffffffffffffffffffff7cca23e9c44edb49aed63690216cc2728dc58f552378c292ab5844f3
{-# INLINE _n #-}

-- | Characteristic of Ed448 curve.
_p :: Integer
_p = 0xfffffffffffffffffffffffffffffffffffffffffffffffffffffffeffffffffffffffffffffffffffffffffffffffffffffffffffffffff
{-# INLINE _p #-}

-- | Coordinate @X@ of Ed448 curve.
_x :: Fp
_x = 0x297ea0ea2692ff1b4faff46098453a6a26adf733245f065c3c59d0709cecfa96147eaaf3932d94c63d96c170033f4ba0c7f0de840aed939f
{-# INLINE _x #-}

-- | Coordinate @Y@ of Ed448 curve.
_y :: Fp
_y = 0x13
{-# INLINE _y #-}
