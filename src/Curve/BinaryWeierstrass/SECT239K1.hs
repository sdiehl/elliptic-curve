module Curve.BinaryWeierstrass.SECT239K1
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

-- | SECT239K1 curve
data SECT239K1

-- | Polynomial of SECT239K1 curve
data FX
instance IrreducibleMonic F2 FX where
  split _ = x ^ (239 :: Int) + x ^ (158 :: Int) + 1

-- | Field of SECT239K1 curve
type F2m = Fm FX

-- | SECT239K1 curve is a binary Weierstrass curve
instance BWCurve SECT239K1 FX where
  _a _ = 0
  {-# INLINE _a #-}
  _b _ = 1
  {-# INLINE _b #-}
  _f _ = witness :: FX
  {-# INLINE _f #-}
  _g   = notImplemented
  {-# INLINE _g #-}
  _h _ = 4
  {-# INLINE _h #-}
  _r _ = 0x2000000000000000000000000000005A79FEC67CB6E91F1C1DA800E478A5
  {-# INLINE _r #-}

-- | Point of SECT239K1 curve
type P = BWPoint SECT239K1 F2m
