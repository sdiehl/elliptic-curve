module Curve.Binary.SECT193R1
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

-- | SECT193R1 curve.
data SECT193R1

-- | Field of points of SECT193R1 curve.
type F2m = BinaryField 0x2000000000000000000000000000000000000000000008001

-- | Field of coefficients of SECT193R1 curve.
type Fr = PrimeField 0x1000000000000000000000000c7f34a778f443acc920eba49

-- | SECT193R1 curve is a binary curve.
instance Curve 'Binary c SECT193R1 F2m => BCurve c SECT193R1 F2m where
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

-- | Affine SECT193R1 curve point.
type PA = BAPoint SECT193R1 F2m

-- | Affine SECT193R1 curve is a binary affine curve.
instance BACurve SECT193R1 F2m where
  gA_ = gA
  {-# INLINE gA_ #-}

-- | Projective SECT193R1 point.
type PP = BPPoint SECT193R1 F2m

-- | Projective SECT193R1 curve is a binary projective curve.
instance BPCurve SECT193R1 F2m where
  gP_ = gP
  {-# INLINE gP_ #-}

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

-- | Cofactor of SECT193R1 curve.
_h :: Integer
_h = 0x2
{-# INLINE _h #-}

-- | Polynomial of SECT193R1 curve.
_p :: Integer
_p = 0x2000000000000000000000000000000000000000000008001
{-# INLINE _p #-}

-- | Order of SECT193R1 curve.
_r :: Integer
_r = 0x1000000000000000000000000c7f34a778f443acc920eba49
{-# INLINE _r #-}

-- | Coordinate @X@ of SECT193R1 curve.
_x :: F2m
_x = 0x1f481bc5f0ff84a74ad6cdf6fdef4bf6179625372d8c0c5e1
{-# INLINE _x #-}

-- | Coordinate @Y@ of SECT193R1 curve.
_y :: F2m
_y = 0x25e399f2903712ccf3ea9e3a1ad17fb0b3201b6af7ce1b05
{-# INLINE _y #-}

-- | Generator of affine SECT193R1 curve.
gA :: PA
gA = A _x _y
{-# INLINE gA #-}

-- | Generator of projective SECT193R1 curve.
gP :: PP
gP = P _x _y 1
{-# INLINE gP #-}
