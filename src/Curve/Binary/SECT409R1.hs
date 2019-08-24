module Curve.Binary.SECT409R1
  ( module Curve.Binary
  , module Curve.Binary.SECT409R1
  , Point(..)
  ) where

import Protolude

import Data.Field.Galois

import Curve.Binary

-------------------------------------------------------------------------------
-- Types
-------------------------------------------------------------------------------

-- | SECT409R1 curve.
data SECT409R1

-- | Field of points of SECT409R1 curve.
type F2m = Binary 0x2000000000000000000000000000000000000000000000000000000000000000000000000000000008000000000000000000001

-- | Field of coefficients of SECT409R1 curve.
type Fr = Prime 0x10000000000000000000000000000000000000000000000000001e2aad6a612f33307be5fa47c3c9e052f838164cd37d9a21173

-- | SECT409R1 curve is a binary curve.
instance Curve 'Binary c SECT409R1 F2m Fr => BCurve c SECT409R1 F2m Fr where
  a_ = const _a
  {-# INLINABLE a_ #-}
  b_ = const _b
  {-# INLINABLE b_ #-}
  h_ = const _h
  {-# INLINABLE h_ #-}
  p_ = const _p
  {-# INLINABLE p_ #-}
  r_ = const _r
  {-# INLINABLE r_ #-}
  x_ = const _x
  {-# INLINABLE x_ #-}
  y_ = const _y
  {-# INLINABLE y_ #-}

-- | Affine SECT409R1 curve point.
type PA = BAPoint SECT409R1 F2m Fr

-- | Affine SECT409R1 curve is a binary affine curve.
instance BACurve SECT409R1 F2m Fr where
  gA_ = gA
  {-# INLINABLE gA_ #-}

-- | Projective SECT409R1 point.
type PP = BPPoint SECT409R1 F2m Fr

-- | Projective SECT409R1 curve is a binary projective curve.
instance BPCurve SECT409R1 F2m Fr where
  gP_ = gP
  {-# INLINABLE gP_ #-}

-------------------------------------------------------------------------------
-- Parameters
-------------------------------------------------------------------------------

-- | Coefficient @A@ of SECT409R1 curve.
_a :: F2m
_a = 0x1
{-# INLINABLE _a #-}

-- | Coefficient @B@ of SECT409R1 curve.
_b :: F2m
_b = 0x21a5c2c8ee9feb5c4b9a753b7b476b7fd6422ef1f3dd674761fa99d6ac27c8a9a197b272822f6cd57a55aa4f50ae317b13545f
{-# INLINABLE _b #-}

-- | Cofactor of SECT409R1 curve.
_h :: Integer
_h = 0x2
{-# INLINABLE _h #-}

-- | Polynomial of SECT409R1 curve.
_p :: Integer
_p = 0x2000000000000000000000000000000000000000000000000000000000000000000000000000000008000000000000000000001
{-# INLINABLE _p #-}

-- | Order of SECT409R1 curve.
_r :: Integer
_r = 0x10000000000000000000000000000000000000000000000000001e2aad6a612f33307be5fa47c3c9e052f838164cd37d9a21173
{-# INLINABLE _r #-}

-- | Coordinate @X@ of SECT409R1 curve.
_x :: F2m
_x = 0x15d4860d088ddb3496b0c6064756260441cde4af1771d4db01ffe5b34e59703dc255a868a1180515603aeab60794e54bb7996a7
{-# INLINABLE _x #-}

-- | Coordinate @Y@ of SECT409R1 curve.
_y :: F2m
_y = 0x61b1cfab6be5f32bbfa78324ed106a7636b9c5a7bd198d0158aa4f5488d08f38514f1fdf4b4f40d2181b3681c364ba0273c706
{-# INLINABLE _y #-}

-- | Generator of affine SECT409R1 curve.
gA :: PA
gA = A _x _y
{-# INLINABLE gA #-}

-- | Generator of projective SECT409R1 curve.
gP :: PP
gP = P _x _y 1
{-# INLINABLE gP #-}
