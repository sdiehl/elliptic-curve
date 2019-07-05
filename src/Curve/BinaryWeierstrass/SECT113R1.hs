module Curve.BinaryWeierstrass.SECT113R1
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

-- | SECT113R1 curve
data SECT113R1

-- | Field of SECT113R1 curve
data FX
instance IrreducibleMonic F2 FX where
  split _ = x ^ (113 :: Int) + x ^ (9 :: Int) + 1
type F2m = Fm FX

-- | SECT113R1 curve is a binary Weierstrass curve
instance BWCurve SECT113R1 FX where
  _a _ = 0x003088250ca6e7c7fe649ce85820f7
  {-# INLINE _a #-}
  _b _ = 0x00e8bee4d3e2260744188be0e9c723
  {-# INLINE _b #-}
  _f _ = witness :: FX
  {-# INLINE _f #-}
  _g   = A 0x009d73616f35f4ab1407d73562c10f
           0x00a52830277958ee84d1315ed31886
  {-# INLINE _g #-}
  _h _ = 2
  {-# INLINE _h #-}
  _n _ = 0x0100000000000000d9ccec8a39e56f
  {-# INLINE _n #-}

-- | Point of SECT113R1 curve
type P = BWPoint SECT113R1 F2m
