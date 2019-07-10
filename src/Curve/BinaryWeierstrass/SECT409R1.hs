module Curve.BinaryWeierstrass.SECT409R1
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

-- | SECT409R1 curve
data SECT409R1

-- | Field of SECT409R1 curve
type F2m = BinaryField 0x2000000000000000000000000000000000000000000000000000000000000000000000000000000008000000000000000000001

-- | SECT409R1 curve is a binary Weierstrass curve
instance BWCurve SECT409R1 F2m where
  a_ = const _a
  {-# INLINE a_ #-}
  b_ = const _b
  {-# INLINE b_ #-}
  g_ = _g
  {-# INLINE g_ #-}

-- | Point of SECT409R1 curve
type P = BWPoint SECT409R1 F2m

-------------------------------------------------------------------------------
-- Parameters
-------------------------------------------------------------------------------

-- | Coefficient @A@ of SECT409R1 curve
_a :: F2m
_a = 1
{-# INLINE _a #-}

-- | Coefficient @B@ of SECT409R1 curve
_b :: F2m
_b = 0x21a5c2c8ee9feb5c4b9a753b7b476b7fd6422ef1f3dd674761fa99d6ac27c8a9a197b272822f6cd57a55aa4f50ae317b13545f
{-# INLINE _b #-}

-- | Polynomial of SECT409R1 curve
_f :: Integer
_f = 0x2000000000000000000000000000000000000000000000000000000000000000000000000000000008000000000000000000001
{-# INLINE _f #-}

-- | Generator of SECT409R1 curve
_g :: P
_g = A
     0x15d4860d088ddb3496b0c6064756260441cde4af1771d4db01ffe5b34e59703dc255a868a1180515603aeab60794e54bb7996a7
     0x61b1cfab6be5f32bbfa78324ed106a7636b9c5a7bd198d0158aa4f5488d08f38514f1fdf4b4f40d2181b3681c364ba0273c706
{-# INLINE _g #-}

-- | Cofactor of SECT409R1 curve
_h :: Integer
_h = 2
{-# INLINE _h #-}

-- | Order of SECT409R1 curve
_n :: Integer
_n = 0x10000000000000000000000000000000000000000000000000001e2aad6a612f33307be5fa47c3c9e052f838164cd37d9a21173
{-# INLINE _n #-}
