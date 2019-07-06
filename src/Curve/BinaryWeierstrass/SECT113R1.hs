module Curve.BinaryWeierstrass.SECT113R1
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

-- | SECT113R1 curve
data SECT113R1

-- | Field of SECT113R1 curve
data FX
instance IrreducibleMonic F2 FX where
  split _ = x ^ (113 :: Int) + x ^ (9 :: Int) + 1
type F2m = Fm FX

-- | SECT113R1 curve is a binary Weierstrass curve
instance BWCurve SECT113R1 FX where
  a_ = const _a
  {-# INLINE a_ #-}
  b_ = const _b
  {-# INLINE b_ #-}
  g_ = _g
  {-# INLINE g_ #-}

-- | Point of SECT113R1 curve
type P = BWPoint SECT113R1 F2m

-------------------------------------------------------------------------------
-- Parameters
-------------------------------------------------------------------------------

-- | Coefficient @A@ of SECT113R1 curve
_a :: F2m
_a = 0x003088250ca6e7c7fe649ce85820f7
{-# INLINE _a #-}

-- | Coefficient @B@ of SECT113R1 curve
_b :: F2m
_b = 0x00e8bee4d3e2260744188be0e9c723
{-# INLINE _b #-}

-- | Polynomial of SECT113R1 curve
_f = split (witness :: F2m)
{-# INLINE _f #-}

-- | Generator of SECT113R1 curve
_g :: P
_g = A 0x009d73616f35f4ab1407d73562c10f
       0x00a52830277958ee84d1315ed31886
{-# INLINE _g #-}

-- | Cofactor of SECT113R1 curve
_h :: Integer
_h = 2
{-# INLINE _h #-}

-- | Order of SECT113R1 curve
_n :: Integer
_n = 0x0100000000000000d9ccec8a39e56f
{-# INLINE _n #-}
