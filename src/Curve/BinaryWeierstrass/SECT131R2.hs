module Curve.BinaryWeierstrass.SECT131R2
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

import ExtensionField (IrreducibleMonic(..), x)

import Curve.BinaryWeierstrass (BWCurve(..), BWPoint, F2, Fm, Point(..))

-------------------------------------------------------------------------------
-- Types
-------------------------------------------------------------------------------

-- | SECT131R2 curve
data SECT131R2

-- | Field of SECT131R2 curve
data FX
instance IrreducibleMonic F2 FX where
  split _ = x ^ (131 :: Int) + x ^ (8 :: Int) + x ^ (3 :: Int) + x ^ (2 :: Int) + 1
type F2m = Fm FX

-- | SECT131R2 curve is a binary Weierstrass curve
instance BWCurve SECT131R2 FX where
  a_ = const _a 
  {-# INLINE a_ #-}
  b_ = const _b
  {-# INLINE b_ #-}
  g_ = _g
  {-# INLINE g_ #-}

-- | Point of SECT131R2 curve
type P = BWPoint SECT131R2 F2m

-------------------------------------------------------------------------------
-- Parameters
-------------------------------------------------------------------------------

-- | Coefficient @A@ of SECT131R2 curve
_a :: F2m
_a = 0x03e5a88919d7cafcbf415f07c2176573b2
{-# INLINE _a #-}

-- | Coefficient @B@ of SECT131R2 curve
_b :: F2m
_b = 0x04b8266a46c55657ac734ce38f018f2192
{-# INLINE _b #-}

-- | Polynomial of SECT131R2 curve
_f = split (witness :: F2m)
{-# INLINE _f #-}

-- | Generator of SECT131R2 curve
_g :: P
_g = A 0x0356dcd8f2f95031ad652d23951bb366a8
       0x0648f06d867940a5366d9e265de9eb240f
{-# INLINE _g #-}

-- | Cofactor of SECT131R2 curve
_h :: Integer
_h = 2
{-# INLINE _h #-}

-- | Order of SECT131R2 curve
_n :: Integer
_n = 0x0400000000000000016954a233049ba98f
{-# INLINE _n #-}
