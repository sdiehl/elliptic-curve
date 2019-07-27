module Curve.Edwards.E222
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

import Curve (Curve(..), Group(..))
import Curve.Edwards (ECurve(..), EPoint, Point(..))

-------------------------------------------------------------------------------
-- Types
-------------------------------------------------------------------------------

-- | E222 curve.
data E222

-- | Field of E222 curve.
type Fp = PrimeField 0x3fffffffffffffffffffffffffffffffffffffffffffffffffffff8b

-- | E222 curve is an Edwards curve.
instance ECurve E222 Fp where
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

-- | Point of E222 curve.
type P = EPoint E222 Fp

-------------------------------------------------------------------------------
-- Parameters
-------------------------------------------------------------------------------

-- | Coefficient @A@ of E222 curve.
_a :: Fp
_a = 0x1
{-# INLINE _a #-}

-- | Coefficient @B@ of E222 curve.
_d :: Fp
_d = 0x27166
{-# INLINE _d #-}

-- | Generator of E222 curve.
_g :: P
_g = A _x _y
{-# INLINE _g #-}

-- | Cofactor of E222 curve.
_h :: Integer
_h = 0x4
{-# INLINE _h #-}

-- | Order of E222 curve.
_n :: Integer
_n = 0xffffffffffffffffffffffffffff70cbc95e932f802f31423598cbf
{-# INLINE _n #-}

-- | Characteristic of E222 curve.
_p :: Integer
_p = 0x3fffffffffffffffffffffffffffffffffffffffffffffffffffff8b
{-# INLINE _p #-}

-- | Coordinate @X@ of E222 curve.
_x :: Fp
_x = 0x19b12bb156a389e55c9768c303316d07c23adab3736eb2bc3eb54e51
{-# INLINE _x #-}

-- | Coordinate @Y@ of E222 curve.
_y :: Fp
_y = 0x1c
{-# INLINE _y #-}
