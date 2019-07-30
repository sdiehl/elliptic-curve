module Curve.Binary.SECT113R2
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
-- SECT113R2 curve
-------------------------------------------------------------------------------

-- | SECT113R2 curve.
data SECT113R2

-- | Field of points of SECT113R2 curve.
type F2m = BinaryField 0x20000000000000000000000000201

-- | Field of coefficients of SECT113R2 curve.
type Fr = PrimeField 0x10000000000000108789b2496af93

-- | SECT113R2 curve is a binary curve.
instance Curve 'Binary c SECT113R2 F2m => BCurve c SECT113R2 F2m where
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

-- | Coefficient @A@ of SECT113R2 curve.
_a :: F2m
_a = 0x689918dbec7e5a0dd6dfc0aa55c7
{-# INLINE _a #-}

-- | Coefficient @B@ of SECT113R2 curve.
_b :: F2m
_b = 0x95e9a9ec9b297bd4bf36e059184f
{-# INLINE _b #-}

-- | Cofactor of SECT113R2 curve.
_h :: Integer
_h = 0x2
{-# INLINE _h #-}

-- | Polynomial of SECT113R2 curve.
_p :: Integer
_p = 0x20000000000000000000000000201
{-# INLINE _p #-}

-- | Order of SECT113R2 curve.
_r :: Integer
_r = 0x10000000000000108789b2496af93
{-# INLINE _r #-}

-------------------------------------------------------------------------------
-- Affine coordinates
-------------------------------------------------------------------------------

-- | Affine SECT113R2 point.
type AP = BAPoint SECT113R2 F2m

-- | Affine SECT113R2 curve is a binary affine curve.
instance BACurve SECT113R2 F2m where
  gA_ = gA
  {-# INLINE gA_ #-}
  xA_ = const xA
  {-# INLINE xA_ #-}
  yA_ = const yA
  {-# INLINE yA_ #-}

-- | Generator of affine SECT113R2 curve.
gA :: AP
gA = A xA yA
{-# INLINE gA #-}

-- | Coordinate @X@ of affine SECT113R2 curve.
xA :: F2m
xA = 0x1a57a6a7b26ca5ef52fcdb8164797
{-# INLINE xA #-}

-- | Coordinate @Y@ of affine SECT113R2 curve.
yA :: F2m
yA = 0xb3adc94ed1fe674c06e695baba1d
{-# INLINE yA #-}
