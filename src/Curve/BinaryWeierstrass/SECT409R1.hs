module Curve.BinaryWeierstrass.SECT409R1
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

-- | SECT409R1 curve
data SECT409R1

-- | Field of SECT409R1 curve
data FX
instance IrreducibleMonic F2 FX where
  split _ = x ^ (409 :: Int) + x ^ (87 :: Int) + 1
type F2m = Fm FX

-- | SECT409R1 curve is a binary Weierstrass curve
instance BWCurve SECT409R1 FX where
  _a _ = 1
  {-# INLINE _a #-}
  _b _ = 0x0021a5c2c8ee9feb5c4b9a753b7b476b7fd6422ef1f3dd674761fa99d6ac27c8a9a197b272822f6cd57a55aa4f50ae317b13545f
  {-# INLINE _b #-}
  _f _ = witness :: FX
  {-# INLINE _f #-}
  _g   = A 0x015d4860d088ddb3496b0c6064756260441cde4af1771d4db01ffe5b34e59703dc255a868a1180515603aeab60794e54bb7996a7
           0x0061b1cfab6be5f32bbfa78324ed106a7636b9c5a7bd198d0158aa4f5488d08f38514f1fdf4b4f40d2181b3681c364ba0273c706
  {-# INLINE _g #-}
  _h _ = 2
  {-# INLINE _h #-}
  _n _ = 0x010000000000000000000000000000000000000000000000000001e2aad6a612f33307be5fa47c3c9e052f838164cd37d9a21173
  {-# INLINE _n #-}

-- | Point of SECT409R1 curve
type P = BWPoint SECT409R1 F2m
