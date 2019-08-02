module Curve.Binary.SECT113R2
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
  , Point(..)
  , _a
  , _b
  , _h
  , _p
  , _r
  , _x
  , _y
  , fromAtoP
  , fromPtoA
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
  x_ = const _x
  {-# INLINE x_ #-}
  y_ = const _y
  {-# INLINE y_ #-}

-- | Affine SECT113R2 curve point.
type PA = BAPoint SECT113R2 F2m

-- | Affine SECT113R2 curve is a binary affine curve.
instance BACurve SECT113R2 F2m where
  gA_ = gA
  {-# INLINE gA_ #-}

-- | Projective SECT113R2 point.
type PP = BPPoint SECT113R2 F2m

-- | Projective SECT113R2 curve is a binary projective curve.
instance BPCurve SECT113R2 F2m where
  gP_ = gP
  {-# INLINE gP_ #-}

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

-- | Coordinate @X@ of SECT113R2 curve.
_x :: F2m
_x = 0x1a57a6a7b26ca5ef52fcdb8164797
{-# INLINE _x #-}

-- | Coordinate @Y@ of SECT113R2 curve.
_y :: F2m
_y = 0xb3adc94ed1fe674c06e695baba1d
{-# INLINE _y #-}

-- | Generator of affine SECT113R2 curve.
gA :: PA
gA = A _x _y
{-# INLINE gA #-}

-- | Generator of projective SECT113R2 curve.
gP :: PP
gP = P _x _y 1
{-# INLINE gP #-}
