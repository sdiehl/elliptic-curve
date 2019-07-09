module Curve.BinaryWeierstrass.SECT131R1
  -- | Types
  ( F2m
  , P
  -- | Parameters
  , _a
  , _b
  , _f
  , _g
  , _h
  , _n
  ) where

import Protolude

import BinaryField (BinaryField)

import Curve.BinaryWeierstrass (BWCurve(..), BWPoint, Point(..))

-------------------------------------------------------------------------------
-- Types
-------------------------------------------------------------------------------

-- | SECT131R1 curve
data SECT131R1

-- | Field of SECT131R1 curve
type F2m = BinaryField 0x80000000000000000000000000000010d

-- | SECT131R1 curve is a binary Weierstrass curve
instance BWCurve SECT131R1 F2m where
  a_ = const _a 
  {-# INLINE a_ #-}
  b_ = const _b
  {-# INLINE b_ #-}
  g_ = _g
  {-# INLINE g_ #-}

-- | Point of SECT131R1 curve
type P = BWPoint SECT131R1 F2m

-------------------------------------------------------------------------------
-- Parameters
-------------------------------------------------------------------------------

-- | Coefficient @A@ of SECT131R1 curve
_a :: F2m
_a = 0x07a11b09a76b562144418ff3ff8c2570b8
{-# INLINE _a #-}

-- | Coefficient @B@ of SECT131R1 curve
_b :: F2m
_b = 0x0217c05610884b63b9c6c7291678f9d341
{-# INLINE _b #-}

-- | Polynomial of SECT131R1 curve
_f :: Integer
_f = 0x80000000000000000000000000000010d
{-# INLINE _f #-}

-- | Generator of SECT131R1 curve
_g :: P
_g = A
     0x0081baf91fdf9833c40f9c181343638399
     0x078c6e7ea38c001f73c8134b1b4ef9e150
{-# INLINE _g #-}

-- | Cofactor of SECT131R1 curve
_h :: Integer
_h = 2
{-# INLINE _h #-}

-- | Order of SECT131R1 curve
_n :: Integer
_n = 0x0400000000000000023123953a9464b54d
{-# INLINE _n #-}
