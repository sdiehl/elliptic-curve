module Curve.Binary.SECT193R2
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
-- SECT193R2 curve
-------------------------------------------------------------------------------

-- | SECT193R2 curve.
data SECT193R2

-- | Field of points of SECT193R2 curve.
type F2m = BinaryField 0x2000000000000000000000000000000000000000000008001

-- | Field of coefficients of SECT193R2 curve.
type Fr = PrimeField 0x10000000000000000000000015aab561b005413ccd4ee99d5

-- | SECT193R2 curve is a binary curve.
instance Curve 'Binary c SECT193R2 F2m => BCurve c SECT193R2 F2m where
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

-- | Coefficient @A@ of SECT193R2 curve.
_a :: F2m
_a = 0x163f35a5137c2ce3ea6ed8667190b0bc43ecd69977702709b
{-# INLINE _a #-}

-- | Coefficient @B@ of SECT193R2 curve.
_b :: F2m
_b = 0xc9bb9e8927d4d64c377e2ab2856a5b16e3efb7f61d4316ae
{-# INLINE _b #-}

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

-------------------------------------------------------------------------------
-- Affine coordinates
-------------------------------------------------------------------------------

-- | Affine SECT193R2 point.
type AP = BAPoint SECT193R2 F2m

-- | Affine SECT193R2 curve is a binary affine curve.
instance BACurve SECT193R2 F2m where
  gA_ = gA
  {-# INLINE gA_ #-}
  xA_ = const xA
  {-# INLINE xA_ #-}
  yA_ = const yA
  {-# INLINE yA_ #-}

-- | Generator of affine SECT193R2 curve.
gA :: AP
gA = A xA yA
{-# INLINE gA #-}

-- | Coordinate @X@ of affine SECT193R2 curve.
xA :: F2m
xA = 0xd9b67d192e0367c803f39e1a7e82ca14a651350aae617e8f
{-# INLINE xA #-}

-- | Coordinate @Y@ of affine SECT193R2 curve.
yA :: F2m
yA = 0x1ce94335607c304ac29e7defbd9ca01f596f927224cdecf6c
{-# INLINE yA #-}
