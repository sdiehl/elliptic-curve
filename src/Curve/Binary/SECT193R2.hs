module Curve.Binary.SECT193R2
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

-- | SECT193R2 curve.
data SECT193R2

-- | Field of points of SECT193R2 curve.
type F2m = BinaryField 0x2000000000000000000000000000000000000000000008001

-- | Field of coefficients of SECT193R2 curve.
type Fr = PrimeField 0x10000000000000000000000015aab561b005413ccd4ee99d5

-- | SECT193R2 curve is a binary curve.
instance BCurve SECT193R2 F2m where
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

-- | Point of SECT193R2 curve.
type P = BPoint SECT193R2 F2m

-------------------------------------------------------------------------------
-- Parameters
-------------------------------------------------------------------------------

-- | Coefficient @A@ of SECT193R2 curve.
_a :: F2m
_a = 0x163f35a5137c2ce3ea6ed8667190b0bc43ecd69977702709b
{-# INLINE _a #-}

-- | Coefficient @B@ of SECT193R2 curve.
_b :: F2m
_b = 0xc9bb9e8927d4d64c377e2ab2856a5b16e3efb7f61d4316ae
{-# INLINE _b #-}

-- | Generator of SECT193R2 curve.
_g :: P
_g = A _x _y
{-# INLINE _g #-}

-- | Cofactor of SECT193R2 curve.
_h :: Integer
_h = 0x2
{-# INLINE _h #-}

-- | Polynomial of SECT193R2 curve.
_p :: Integer
_p = 0x2000000000000000000000000000000000000000000008001
{-# INLINE _p #-}

-- | Order of SECT193R2 curve.
_r :: Integer
_r = 0x10000000000000000000000015aab561b005413ccd4ee99d5
{-# INLINE _r #-}

-- | Coordinate @X@ of SECT193R2 curve.
_x :: F2m
_x = 0xd9b67d192e0367c803f39e1a7e82ca14a651350aae617e8f
{-# INLINE _x #-}

-- | Coordinate @Y@ of SECT193R2 curve.
_y :: F2m
_y = 0x1ce94335607c304ac29e7defbd9ca01f596f927224cdecf6c
{-# INLINE _y #-}
