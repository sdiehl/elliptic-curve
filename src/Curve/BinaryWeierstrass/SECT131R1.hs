module Curve.BinaryWeierstrass.SECT131R1
  -- | Imports
  ( BWCurve(..)
  , BWPoint
  , F2
  , Fm
  , Point(..)
  -- | Types
  , F2m
  , P
  ) where

import Protolude

import ExtensionField (IrreducibleMonic(..), x)

import Curve.BinaryWeierstrass (BWCurve(..), BWPoint, F2, Fm, Point(..))

-------------------------------------------------------------------------------
-- Types
-------------------------------------------------------------------------------

-- | SECT131R1 curve
data SECT131R1

-- | Field of SECT131R1 curve
data FX
instance IrreducibleMonic F2 FX where
  split _ = x ^ (131 :: Int) + x ^ (8 :: Int) + x ^ (3 :: Int) + x ^ (2 :: Int) + 1
type F2m = Fm FX

-- | SECT131R1 curve is a binary Weierstrass curve
instance BWCurve SECT131R1 FX where
  _a _ = 0x07a11b09a76b562144418ff3ff8c2570b8
  {-# INLINE _a #-}
  _b _ = 0x0217c05610884b63b9c6c7291678f9d341
  {-# INLINE _b #-}
  _f _ = witness :: FX
  {-# INLINE _f #-}
  _g   = A 0x0081baf91fdf9833c40f9c181343638399
           0x078c6e7ea38c001f73c8134b1b4ef9e150
  {-# INLINE _g #-}
  _h _ = 2
  {-# INLINE _h #-}
  _n _ = 0x0400000000000000023123953a9464b54d
  {-# INLINE _n #-}

-- | Point of SECT131R1 curve
type P = BWPoint SECT131R1 F2m
