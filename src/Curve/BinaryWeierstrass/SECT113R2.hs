module Curve.BinaryWeierstrass.SECT113R2
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

-- | SECT113R2 curve
data SECT113R2

-- | Polynomial of SECT113R2 curve
data FX
instance IrreducibleMonic F2 FX where
  split _ = x ^ (113 :: Int) + x ^ (9 :: Int) + 1

-- | Field of SECT113R2 curve
type F2m = Fm FX

-- | SECT113R2 curve is a binary Weierstrass curve
instance BWCurve SECT113R2 FX where
  _a _ = 0x00689918DBEC7E5A0DD6DFC0AA55C7
  {-# INLINE _a #-}
  _b _ = 0x0095E9A9EC9B297BD4BF36E059184F
  {-# INLINE _b #-}
  _f _ = witness :: FX
  {-# INLINE _f #-}
  _g   = notImplemented
  {-# INLINE _g #-}
  _h _ = 2
  {-# INLINE _h #-}
  _r _ = 0x010000000000000108789B2496AF93
  {-# INLINE _r #-}

-- | Point of SECT113R2 curve
type P = BWPoint SECT113R2 F2m
