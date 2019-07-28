module Curve.Binary.SECT113R2
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

-- | SECT113R2 curve.
data SECT113R2

-- | Field of SECT113R2 curve.
type F2m = BinaryField 0x20000000000000000000000000201

-- | SECT113R2 curve is a binary curve.
instance BCurve SECT113R2 F2m where
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

-- | Point of SECT113R2 curve.
type P = BPoint SECT113R2 F2m

-------------------------------------------------------------------------------
-- Parameters
-------------------------------------------------------------------------------

-- | Coefficient @A@ of SECT113R2 curve.
_a :: F2m
_a = 0x689918dbec7e5a0dd6dfc0aa55c7
{-# INLINE _a #-}

-- | Coefficient @B@ of SECT113R2 curve.
_b :: F2m
_b = 0x95e9a9ec9b297bd4bf36e059184f
{-# INLINE _b #-}

-- | Generator of SECT113R2 curve.
_g :: P
_g = A _x _y
{-# INLINE _g #-}

-- | Cofactor of SECT113R2 curve.
_h :: Integer
_h = 0x2
{-# INLINE _h #-}

-- | Order of SECT113R2 curve.
_n :: Integer
_n = 0x10000000000000108789b2496af93
{-# INLINE _n #-}

-- | Polynomial of SECT113R2 curve.
_p :: Integer
_p = 0x20000000000000000000000000201
{-# INLINE _p #-}

-- | Coordinate @X@ of SECT113R2 curve.
_x :: F2m
_x = 0x1a57a6a7b26ca5ef52fcdb8164797
{-# INLINE _x #-}

-- | Coordinate @Y@ of SECT113R2 curve.
_y :: F2m
_y = 0xb3adc94ed1fe674c06e695baba1d
{-# INLINE _y #-}
