module Curve.Binary.SECT113R1
  ( AP
  , BCurve(..)
  , BPoint
  , BACurve(..)
  , BAPoint
  , Curve(..)
  , F2m
  , Fr
  , Group(..)
  , Point(..)
  , _a
  , _b
  , _h
  , _p
  , _r
  , gA
  , xA
  , yA
  ) where

import Protolude

import BinaryField (BinaryField)
import PrimeField (PrimeField)

import Curve (Curve(..), Form(..))
import Curve.Binary (BCurve(..), BPoint, BACurve(..), BAPoint, Point(..))
import Group (Group(..))

-------------------------------------------------------------------------------
-- SECT113R1 curve
-------------------------------------------------------------------------------

-- | SECT113R1 curve.
data SECT113R1

-- | Field of points of SECT113R1 curve.
type F2m = BinaryField 0x20000000000000000000000000201

-- | Field of coefficients of SECT113R1 curve.
type Fr = PrimeField 0x100000000000000d9ccec8a39e56f

-- | SECT113R1 curve is a binary curve.
instance Curve 'Binary c SECT113R1 F2m => BCurve c SECT113R1 F2m where
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

-- | Coefficient @A@ of SECT113R1 curve.
_a :: F2m
_a = 0x3088250ca6e7c7fe649ce85820f7
{-# INLINE _a #-}

-- | Coefficient @B@ of SECT113R1 curve.
_b :: F2m
_b = 0xe8bee4d3e2260744188be0e9c723
{-# INLINE _b #-}

-- | Cofactor of SECT113R1 curve.
_h :: Integer
_h = 0x2
{-# INLINE _h #-}

-- | Polynomial of SECT113R1 curve.
_p :: Integer
_p = 0x20000000000000000000000000201
{-# INLINE _p #-}

-- | Order of SECT113R1 curve.
_r :: Integer
_r = 0x100000000000000d9ccec8a39e56f
{-# INLINE _r #-}

-------------------------------------------------------------------------------
-- Affine coordinates
-------------------------------------------------------------------------------

-- | Affine SECT113R1 point.
type AP = BAPoint SECT113R1 F2m

-- | Affine SECT113R1 curve is a binary affine curve.
instance BACurve SECT113R1 F2m where
  gA_ = gA
  {-# INLINE gA_ #-}
  xA_ = const xA
  {-# INLINE xA_ #-}
  yA_ = const yA
  {-# INLINE yA_ #-}

-- | Generator of affine SECT113R1 curve.
gA :: AP
gA = A xA yA
{-# INLINE gA #-}

-- | Coordinate @X@ of affine SECT113R1 curve.
xA :: F2m
xA = 0x9d73616f35f4ab1407d73562c10f
{-# INLINE xA #-}

-- | Coordinate @Y@ of affine SECT113R1 curve.
yA :: F2m
yA = 0xa52830277958ee84d1315ed31886
{-# INLINE yA #-}
