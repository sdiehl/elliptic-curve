module Curve.Binary.SECT131R1
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
-- SECT131R1 curve
-------------------------------------------------------------------------------

-- | SECT131R1 curve.
data SECT131R1

-- | Field of points of SECT131R1 curve.
type F2m = BinaryField 0x80000000000000000000000000000010d

-- | Field of coefficients of SECT131R1 curve.
type Fr = PrimeField 0x400000000000000023123953a9464b54d

-- | SECT131R1 curve is a binary curve.
instance Curve 'Binary c SECT131R1 F2m => BCurve c SECT131R1 F2m where
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

-- | Coefficient @A@ of SECT131R1 curve.
_a :: F2m
_a = 0x7a11b09a76b562144418ff3ff8c2570b8
{-# INLINE _a #-}

-- | Coefficient @B@ of SECT131R1 curve.
_b :: F2m
_b = 0x217c05610884b63b9c6c7291678f9d341
{-# INLINE _b #-}

-- | Cofactor of SECT131R1 curve.
_h :: Integer
_h = 0x2
{-# INLINE _h #-}

-- | Polynomial of SECT131R1 curve.
_p :: Integer
_p = 0x80000000000000000000000000000010d
{-# INLINE _p #-}

-- | Order of SECT131R1 curve.
_r :: Integer
_r = 0x400000000000000023123953a9464b54d
{-# INLINE _r #-}

-------------------------------------------------------------------------------
-- Affine coordinates
-------------------------------------------------------------------------------

-- | Affine SECT131R1 point.
type AP = BAPoint SECT131R1 F2m

-- | Affine SECT131R1 curve is a binary affine curve.
instance BACurve SECT131R1 F2m where
  gA_ = gA
  {-# INLINE gA_ #-}
  xA_ = const xA
  {-# INLINE xA_ #-}
  yA_ = const yA
  {-# INLINE yA_ #-}

-- | Generator of affine SECT131R1 curve.
gA :: AP
gA = A xA yA
{-# INLINE gA #-}

-- | Coordinate @X@ of affine SECT131R1 curve.
xA :: F2m
xA = 0x81baf91fdf9833c40f9c181343638399
{-# INLINE xA #-}

-- | Coordinate @Y@ of affine SECT131R1 curve.
yA :: F2m
yA = 0x78c6e7ea38c001f73c8134b1b4ef9e150
{-# INLINE yA #-}
