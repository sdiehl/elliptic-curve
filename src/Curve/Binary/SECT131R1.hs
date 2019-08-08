module Curve.Binary.SECT131R1
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

-- | SECT131R1 curve.
data SECT131R1

-- | Field of points of SECT131R1 curve.
type F2m = BinaryField 0x80000000000000000000000000000010d

-- | Field of coefficients of SECT131R1 curve.
type Fr = PrimeField 0x400000000000000023123953a9464b54d

-- | SECT131R1 curve is a binary curve.
instance Curve 'Binary c SECT131R1 F2m Fr => BCurve c SECT131R1 F2m Fr where
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

-- | Affine SECT131R1 curve point.
type PA = BAPoint SECT131R1 F2m Fr

-- | Affine SECT131R1 curve is a binary affine curve.
instance BACurve SECT131R1 F2m Fr where
  gA_ = gA
  {-# INLINE gA_ #-}

-- | Projective SECT131R1 point.
type PP = BPPoint SECT131R1 F2m Fr

-- | Projective SECT131R1 curve is a binary projective curve.
instance BPCurve SECT131R1 F2m Fr where
  gP_ = gP
  {-# INLINE gP_ #-}

-------------------------------------------------------------------------------
-- Parameters
-------------------------------------------------------------------------------

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

-- | Coordinate @X@ of SECT131R1 curve.
_x :: F2m
_x = 0x81baf91fdf9833c40f9c181343638399
{-# INLINE _x #-}

-- | Coordinate @Y@ of SECT131R1 curve.
_y :: F2m
_y = 0x78c6e7ea38c001f73c8134b1b4ef9e150
{-# INLINE _y #-}

-- | Generator of affine SECT131R1 curve.
gA :: PA
gA = A _x _y
{-# INLINE gA #-}

-- | Generator of projective SECT131R1 curve.
gP :: PP
gP = P _x _y 1
{-# INLINE gP #-}
