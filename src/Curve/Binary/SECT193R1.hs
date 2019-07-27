module Curve.Binary.SECT193R1
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

-- | SECT193R1 curve.
data SECT193R1

-- | Field of SECT193R1 curve.
type F2m = BinaryField 0x2000000000000000000000000000000000000000000008001

-- | SECT193R1 curve is a binary curve.
instance BCurve SECT193R1 F2m where
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

-- | Point of SECT193R1 curve.
type P = BPoint SECT193R1 F2m

-------------------------------------------------------------------------------
-- Parameters
-------------------------------------------------------------------------------

-- | Coefficient @A@ of SECT193R1 curve.
_a :: F2m
_a = 0x17858feb7a98975169e171f77b4087de098ac8a911df7b01
{-# INLINE _a #-}

-- | Coefficient @B@ of SECT193R1 curve.
_b :: F2m
_b = 0xfdfb49bfe6c3a89facadaa7a1e5bbc7cc1c2e5d831478814
{-# INLINE _b #-}

-- | Generator of SECT193R1 curve.
_g :: P
_g = A _x _y
{-# INLINE _g #-}

-- | Cofactor of SECT193R1 curve.
_h :: Integer
_h = 0x2
{-# INLINE _h #-}

-- | Order of SECT193R1 curve.
_n :: Integer
_n = 0x1000000000000000000000000c7f34a778f443acc920eba49
{-# INLINE _n #-}

-- | Polynomial of SECT193R1 curve.
_p :: Integer
_p = 0x2000000000000000000000000000000000000000000008001
{-# INLINE _p #-}

-- | Coordinate @X@ of SECT193R1 curve.
_x :: F2m
_x = 0x1f481bc5f0ff84a74ad6cdf6fdef4bf6179625372d8c0c5e1
{-# INLINE _x #-}

-- | Coordinate @Y@ of SECT193R1 curve.
_y :: F2m
_y = 0x25e399f2903712ccf3ea9e3a1ad17fb0b3201b6af7ce1b05
{-# INLINE _y #-}
