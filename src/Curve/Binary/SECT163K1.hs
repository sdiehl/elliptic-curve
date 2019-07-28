module Curve.Binary.SECT163K1
  ( BCurve(..)
  , BPoint
  , Curve(..)
  , F2m
  , Group(..)
  , P
  , Point(..)
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

import BinaryField (BinaryField)

import Curve (Curve(..))
import Curve.Binary (BCurve(..), BPoint, Point(..))
import Group (Group(..))

-------------------------------------------------------------------------------
-- Types
-------------------------------------------------------------------------------

-- | SECT163K1 curve.
data SECT163K1

-- | Field of SECT163K1 curve.
type F2m = BinaryField 0x800000000000000000000000000000000000000c9

-- | SECT163K1 curve is a binary curve.
instance BCurve SECT163K1 F2m where
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

-- | Point of SECT163K1 curve.
type P = BPoint SECT163K1 F2m

-------------------------------------------------------------------------------
-- Parameters
-------------------------------------------------------------------------------

-- | Coefficient @A@ of SECT163K1 curve.
_a :: F2m
_a = 0x1
{-# INLINE _a #-}

-- | Coefficient @B@ of SECT163K1 curve.
_b :: F2m
_b = 0x1
{-# INLINE _b #-}

-- | Generator of SECT163K1 curve.
_g :: P
_g = A _x _y
{-# INLINE _g #-}

-- | Cofactor of SECT163K1 curve.
_h :: Integer
_h = 0x2
{-# INLINE _h #-}

-- | Order of SECT163K1 curve.
_n :: Integer
_n = 0x4000000000000000000020108a2e0cc0d99f8a5ef
{-# INLINE _n #-}

-- | Polynomial of SECT163K1 curve.
_p :: Integer
_p = 0x800000000000000000000000000000000000000c9
{-# INLINE _p #-}

-- | Coordinate @X@ of SECT163K1 curve.
_x :: F2m
_x = 0x2fe13c0537bbc11acaa07d793de4e6d5e5c94eee8
{-# INLINE _x #-}

-- | Coordinate @Y@ of SECT163K1 curve.
_y :: F2m
_y = 0x289070fb05d38ff58321f2e800536d538ccdaa3d9
{-# INLINE _y #-}
