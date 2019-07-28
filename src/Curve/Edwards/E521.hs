module Curve.Edwards.E521
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

-- | E521 curve.
data E521

-- | Field of E521 curve.
type Fp = PrimeField 0x1ffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff

-- | E521 curve is an Edwards curve.
instance ECurve E521 Fp where
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

-- | Point of E521 curve.
type P = EPoint E521 Fp

-------------------------------------------------------------------------------
-- Parameters
-------------------------------------------------------------------------------

-- | Coefficient @A@ of E521 curve.
_a :: Fp
_a = 0x1
{-# INLINE _a #-}

-- | Coefficient @B@ of E521 curve.
_d :: Fp
_d = 0x1fffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffa4331
{-# INLINE _d #-}

-- | Generator of E521 curve.
_g :: P
_g = A _x _y
{-# INLINE _g #-}

-- | Cofactor of E521 curve.
_h :: Integer
_h = 0x4
{-# INLINE _h #-}

-- | Order of E521 curve.
_n :: Integer
_n = 0x7ffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffd15b6c64746fc85f736b8af5e7ec53f04fbd8c4569a8f1f4540ea2435f5180d6b
{-# INLINE _n #-}

-- | Characteristic of E521 curve.
_p :: Integer
_p = 0x1ffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff
{-# INLINE _p #-}

-- | Coordinate @X@ of E521 curve.
_x :: Fp
_x = 0x752cb45c48648b189df90cb2296b2878a3bfd9f42fc6c818ec8bf3c9c0c6203913f6ecc5ccc72434b1ae949d568fc99c6059d0fb13364838aa302a940a2f19ba6c
{-# INLINE _x #-}

-- | Coordinate @Y@ of E521 curve.
_y :: Fp
_y = 0xc
{-# INLINE _y #-}
