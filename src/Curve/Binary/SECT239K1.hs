module Curve.Binary.SECT239K1
  ( F2m
  , P
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

-- | SECT239K1 curve
data SECT239K1

-- | Field of SECT239K1 curve
type F2m = BinaryField 0x800000000000000000004000000000000000000000000000000000000001

-- | SECT239K1 curve is a binary curve
instance BCurve SECT239K1 F2m where
  a_ = const _a
  {-# INLINE a_ #-}
  b_ = const _b
  {-# INLINE b_ #-}
  g_ = _g
  {-# INLINE g_ #-}

-- | Point of SECT239K1 curve
type P = BPoint SECT239K1 F2m

-------------------------------------------------------------------------------
-- Parameters
-------------------------------------------------------------------------------

-- | Coefficient @A@ of SECT239K1 curve
_a :: F2m
_a = 0
{-# INLINE _a #-}

-- | Coefficient @B@ of SECT239K1 curve
_b :: F2m
_b = 1
{-# INLINE _b #-}

-- | Polynomial of SECT239K1 curve
_f :: Integer
_f = 0x800000000000000000004000000000000000000000000000000000000001
{-# INLINE _f #-}

-- | Generator of SECT239K1 curve
_g :: P
_g = A
     0x29a0b6a887a983e9730988a68727a8b2d126c44cc2cc7b2a6555193035dc
     0x76310804f12e549bdb011c103089e73510acb275fc312a5dc6b76553f0ca
{-# INLINE _g #-}

-- | Cofactor of SECT239K1 curve
_h :: Integer
_h = 4
{-# INLINE _h #-}

-- | Order of SECT239K1 curve
_n :: Integer
_n = 0x2000000000000000000000000000005a79fec67cb6e91f1c1da800e478a5
{-# INLINE _n #-}
