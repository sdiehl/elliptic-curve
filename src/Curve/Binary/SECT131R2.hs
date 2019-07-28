module Curve.Binary.SECT131R2
  ( BCurve(..)
  , BPoint
  , Curve(..)
  , F2m
  , Fr
  , Group(..)
  , P
  , Point(..)
  , _a
  , _b
  , _g
  , _h
  , _p
  , _r
  , _x
  , _y
  ) where

import Protolude

import BinaryField (BinaryField)
import PrimeField (PrimeField)

import Curve (Curve(..))
import Curve.Binary (BCurve(..), BPoint, Point(..))
import Group (Group(..))

-------------------------------------------------------------------------------
-- Types
-------------------------------------------------------------------------------

-- | SECT131R2 curve.
data SECT131R2

-- | Field of points of SECT131R2 curve.
type F2m = BinaryField 0x80000000000000000000000000000010d

-- | Field of coefficients of SECT131R2 curve.
type Fr = PrimeField 0x400000000000000016954a233049ba98f

-- | SECT131R2 curve is a binary curve.
instance BCurve SECT131R2 F2m where
  a_ = const _a
  {-# INLINE a_ #-}
  b_ = const _b
  {-# INLINE b_ #-}
  g_ = _g
  {-# INLINE g_ #-}
  h_ = const _h
  {-# INLINE h_ #-}
  p_ = const _p
  {-# INLINE p_ #-}
  r_ = const _r
  {-# INLINE r_ #-}
  x_ = const _x
  {-# INLINE x_ #-}
  y_ = const _y
  {-# INLINE y_ #-}

-- | Point of SECT131R2 curve.
type P = BPoint SECT131R2 F2m

-------------------------------------------------------------------------------
-- Parameters
-------------------------------------------------------------------------------

-- | Coefficient @A@ of SECT131R2 curve.
_a :: F2m
_a = 0x3e5a88919d7cafcbf415f07c2176573b2
{-# INLINE _a #-}

-- | Coefficient @B@ of SECT131R2 curve.
_b :: F2m
_b = 0x4b8266a46c55657ac734ce38f018f2192
{-# INLINE _b #-}

-- | Generator of SECT131R2 curve.
_g :: P
_g = A _x _y
{-# INLINE _g #-}

-- | Cofactor of SECT131R2 curve.
_h :: Integer
_h = 0x2
{-# INLINE _h #-}

-- | Polynomial of SECT131R2 curve.
_p :: Integer
_p = 0x80000000000000000000000000000010d
{-# INLINE _p #-}

-- | Order of SECT131R2 curve.
_r :: Integer
_r = 0x400000000000000016954a233049ba98f
{-# INLINE _r #-}

-- | Coordinate @X@ of SECT131R2 curve.
_x :: F2m
_x = 0x356dcd8f2f95031ad652d23951bb366a8
{-# INLINE _x #-}

-- | Coordinate @Y@ of SECT131R2 curve.
_y :: F2m
_y = 0x648f06d867940a5366d9e265de9eb240f
{-# INLINE _y #-}
