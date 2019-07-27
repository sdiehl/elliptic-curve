module Curve.Binary.SECT233R1
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

import Curve (Curve(..), Group(..))
import Curve.Binary (BCurve(..), BPoint, Point(..))

-------------------------------------------------------------------------------
-- Types
-------------------------------------------------------------------------------

-- | SECT233R1 curve.
data SECT233R1

-- | Field of SECT233R1 curve.
type F2m = BinaryField 0x20000000000000000000000000000000000000004000000000000000001

-- | SECT233R1 curve is a binary curve.
instance BCurve SECT233R1 F2m where
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

-- | Point of SECT233R1 curve.
type P = BPoint SECT233R1 F2m

-------------------------------------------------------------------------------
-- Parameters
-------------------------------------------------------------------------------

-- | Coefficient @A@ of SECT233R1 curve.
_a :: F2m
_a = 0x1
{-# INLINE _a #-}

-- | Coefficient @B@ of SECT233R1 curve.
_b :: F2m
_b = 0x66647ede6c332c7f8c0923bb58213b333b20e9ce4281fe115f7d8f90ad
{-# INLINE _b #-}

-- | Generator of SECT233R1 curve.
_g :: P
_g = A _x _y
{-# INLINE _g #-}

-- | Cofactor of SECT233R1 curve.
_h :: Integer
_h = 0x2
{-# INLINE _h #-}

-- | Order of SECT233R1 curve.
_n :: Integer
_n = 0x1000000000000000000000000000013e974e72f8a6922031d2603cfe0d7
{-# INLINE _n #-}

-- | Polynomial of SECT233R1 curve.
_p :: Integer
_p = 0x20000000000000000000000000000000000000004000000000000000001
{-# INLINE _p #-}

-- | Coordinate @X@ of SECT233R1 curve.
_x :: F2m
_x = 0xfac9dfcbac8313bb2139f1bb755fef65bc391f8b36f8f8eb7371fd558b
{-# INLINE _x #-}

-- | Coordinate @Y@ of SECT233R1 curve.
_y :: F2m
_y = 0x1006a08a41903350678e58528bebf8a0beff867a7ca36716f7e01f81052
{-# INLINE _y #-}
