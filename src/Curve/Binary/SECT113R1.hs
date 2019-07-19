module Curve.Binary.SECT113R1
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

-- | SECT113R1 curve
data SECT113R1

-- | Field of SECT113R1 curve
type F2m = BinaryField 0x20000000000000000000000000201

-- | SECT113R1 curve is a binary curve
instance BCurve SECT113R1 F2m where
  a_ = const _a
  {-# INLINE a_ #-}
  b_ = const _b
  {-# INLINE b_ #-}
  g_ = _g
  {-# INLINE g_ #-}

-- | Point of SECT113R1 curve
type P = BPoint SECT113R1 F2m

-------------------------------------------------------------------------------
-- Parameters
-------------------------------------------------------------------------------

-- | Coefficient @A@ of SECT113R1 curve
_a :: F2m
_a = 0x3088250ca6e7c7fe649ce85820f7
{-# INLINE _a #-}

-- | Coefficient @B@ of SECT113R1 curve
_b :: F2m
_b = 0xe8bee4d3e2260744188be0e9c723
{-# INLINE _b #-}

-- | Polynomial of SECT113R1 curve
_f :: Integer
_f = 0x20000000000000000000000000201
{-# INLINE _f #-}

-- | Generator of SECT113R1 curve
_g :: P
_g = A
     0x9d73616f35f4ab1407d73562c10f
     0xa52830277958ee84d1315ed31886
{-# INLINE _g #-}

-- | Cofactor of SECT113R1 curve
_h :: Integer
_h = 2
{-# INLINE _h #-}

-- | Order of SECT113R1 curve
_n :: Integer
_n = 0x100000000000000d9ccec8a39e56f
{-# INLINE _n #-}
