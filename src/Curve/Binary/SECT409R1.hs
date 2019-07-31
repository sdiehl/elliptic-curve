module Curve.Binary.SECT409R1
  ( BCurve(..)
  , BPoint
  , BACurve(..)
  , BAPoint
  , BPCurve(..)
  , BPPoint
  , Curve(..)
  , F2m
  , Fr
  , Group(..)
  , PA
  , PP
  , _a
  , _b
  , _h
  , _p
  , _r
  , _x
  , _y
  , gA
  , gP
  ) where

import Protolude

import BinaryField
import PrimeField

import Curve.Binary

-------------------------------------------------------------------------------
-- Types
-------------------------------------------------------------------------------

-- | SECT409R1 curve.
data SECT409R1

-- | Field of points of SECT409R1 curve.
type F2m = BinaryField 0x2000000000000000000000000000000000000000000000000000000000000000000000000000000008000000000000000000001

-- | Field of coefficients of SECT409R1 curve.
type Fr = PrimeField 0x10000000000000000000000000000000000000000000000000001e2aad6a612f33307be5fa47c3c9e052f838164cd37d9a21173

-- | SECT409R1 curve is a binary curve.
instance Curve 'Binary c SECT409R1 F2m => BCurve c SECT409R1 F2m where
  a_ = const _a
  {-# INLINE a_ #-}
  b_ = const _b
  {-# INLINE b_ #-}
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

-- | Affine SECT409R1 curve point.
type PA = BAPoint SECT409R1 F2m

-- | Affine SECT409R1 curve is a binary affine curve.
instance BACurve SECT409R1 F2m where
  gA_ = gA
  {-# INLINE gA_ #-}

-- | Projective SECT409R1 point.
type PP = BPPoint SECT409R1 F2m

-- | Projective SECT409R1 curve is a binary projective curve.
instance BPCurve SECT409R1 F2m where
  gP_ = gP
  {-# INLINE gP_ #-}

-------------------------------------------------------------------------------
-- Parameters
-------------------------------------------------------------------------------

-- | Coefficient @A@ of SECT409R1 curve.
_a :: F2m
_a = 0x1
{-# INLINE _a #-}

-- | Coefficient @B@ of SECT409R1 curve.
_b :: F2m
_b = 0x21a5c2c8ee9feb5c4b9a753b7b476b7fd6422ef1f3dd674761fa99d6ac27c8a9a197b272822f6cd57a55aa4f50ae317b13545f
{-# INLINE _b #-}

-- | Cofactor of SECT409R1 curve.
_h :: Integer
_h = 0x2
{-# INLINE _h #-}

-- | Polynomial of SECT409R1 curve.
_p :: Integer
_p = 0x2000000000000000000000000000000000000000000000000000000000000000000000000000000008000000000000000000001
{-# INLINE _p #-}

-- | Order of SECT409R1 curve.
_r :: Integer
_r = 0x10000000000000000000000000000000000000000000000000001e2aad6a612f33307be5fa47c3c9e052f838164cd37d9a21173
{-# INLINE _r #-}

-- | Coordinate @X@ of SECT409R1 curve.
_x :: F2m
_x = 0x15d4860d088ddb3496b0c6064756260441cde4af1771d4db01ffe5b34e59703dc255a868a1180515603aeab60794e54bb7996a7
{-# INLINE _x #-}

-- | Coordinate @Y@ of SECT409R1 curve.
_y :: F2m
_y = 0x61b1cfab6be5f32bbfa78324ed106a7636b9c5a7bd198d0158aa4f5488d08f38514f1fdf4b4f40d2181b3681c364ba0273c706
{-# INLINE _y #-}

-- | Affine generator of SECT409R1 curve.
gA :: PA
gA = fromMaybe (panic "not well-defined.") (point _x _y)
{-# INLINE gA #-}

-- | Projective generator of SECT409R1 curve.
gP :: PP
gP = fromMaybe (panic "not well-defined.") (point _x _y)
{-# INLINE gP #-}
