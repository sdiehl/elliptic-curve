module Curve.Binary.SECT131R2
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
-- SECT131R2 curve
-------------------------------------------------------------------------------

-- | SECT131R2 curve.
data SECT131R2

-- | Field of points of SECT131R2 curve.
type F2m = BinaryField 0x80000000000000000000000000000010d

-- | Field of coefficients of SECT131R2 curve.
type Fr = PrimeField 0x400000000000000016954a233049ba98f

-- | SECT131R2 curve is a binary curve.
instance Curve 'Binary c SECT131R2 F2m => BCurve c SECT131R2 F2m where
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

-- | Coefficient @A@ of SECT131R2 curve.
_a :: F2m
_a = 0x3e5a88919d7cafcbf415f07c2176573b2
{-# INLINE _a #-}

-- | Coefficient @B@ of SECT131R2 curve.
_b :: F2m
_b = 0x4b8266a46c55657ac734ce38f018f2192
{-# INLINE _b #-}

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

-------------------------------------------------------------------------------
-- Affine coordinates
-------------------------------------------------------------------------------

-- | Affine SECT131R2 point.
type AP = BAPoint SECT131R2 F2m

-- | Affine SECT131R2 curve is a binary affine curve.
instance BACurve SECT131R2 F2m where
  gA_ = gA
  {-# INLINE gA_ #-}
  xA_ = const xA
  {-# INLINE xA_ #-}
  yA_ = const yA
  {-# INLINE yA_ #-}

-- | Generator of affine SECT131R2 curve.
gA :: AP
gA = A xA yA
{-# INLINE gA #-}

-- | Coordinate @X@ of affine SECT131R2 curve.
xA :: F2m
xA = 0x356dcd8f2f95031ad652d23951bb366a8
{-# INLINE xA #-}

-- | Coordinate @Y@ of affine SECT131R2 curve.
yA :: F2m
yA = 0x648f06d867940a5366d9e265de9eb240f
{-# INLINE yA #-}
