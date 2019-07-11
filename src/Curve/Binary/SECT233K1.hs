module Curve.Binary.SECT233K1
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

import Curve.Binary (BCurve(..), BPoint, Point(..))

-------------------------------------------------------------------------------
-- Types
-------------------------------------------------------------------------------

-- | SECT233K1 curve
data SECT233K1

-- | Field of SECT233K1 curve
type F2m = BinaryField 0x20000000000000000000000000000000000000004000000000000000001

-- | SECT233K1 curve is a binary curve
instance BCurve SECT233K1 F2m where
  a_ = const _a
  {-# INLINE a_ #-}
  b_ = const _b
  {-# INLINE b_ #-}
  g_ = _g
  {-# INLINE g_ #-}

-- | Point of SECT233K1 curve
type P = BPoint SECT233K1 F2m

-------------------------------------------------------------------------------
-- Parameters
-------------------------------------------------------------------------------

-- | Coefficient @A@ of SECT233K1 curve
_a :: F2m
_a = 0
{-# INLINE _a #-}

-- | Coefficient @B@ of SECT233K1 curve
_b :: F2m
_b = 1
{-# INLINE _b #-}

-- | Polynomial of SECT233K1 curve
_f :: Integer
_f = 0x20000000000000000000000000000000000000004000000000000000001
{-# INLINE _f #-}

-- | Generator of SECT233K1 curve
_g :: P
_g = A
     0x17232ba853a7e731af129f22ff4149563a419c26bf50a4c9d6eefad6126
     0x1db537dece819b7f70f555a67c427a8cd9bf18aeb9b56e0c11056fae6a3
{-# INLINE _g #-}

-- | Cofactor of SECT233K1 curve
_h :: Integer
_h = 4
{-# INLINE _h #-}

-- | Order of SECT233K1 curve
_n :: Integer
_n = 0x8000000000000000000000000000069d5bb915bcd46efb1ad5f173abdf
{-# INLINE _n #-}