module Curve.BinaryWeierstrass.SECT193R1
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

-- | SECT193R1 curve
data SECT193R1

-- | Polynomial of SECT193R1 curve
data FX
instance IrreducibleMonic F2 FX where
  split _ = x ^ (193 :: Int) + x ^ (15 :: Int) + 1

-- | Field of SECT193R1 curve
type F2m = Fm FX

-- | SECT193R1 curve is a binary Weierstrass curve
instance BWCurve SECT193R1 FX where
  _a _ = 0x0017858FEB7A98975169E171F77B4087DE098AC8A911DF7B01
  {-# INLINE _a #-}
  _b _ = 0x00FDFB49BFE6C3A89FACADAA7A1E5BBC7CC1C2E5D831478814
  {-# INLINE _b #-}
  _f _ = witness :: FX
  {-# INLINE _f #-}
  _g   = notImplemented
  {-# INLINE _g #-}
  _h _ = 2
  {-# INLINE _h #-}
  _r _ = 0x01000000000000000000000000C7F34A778F443ACC920EBA49
  {-# INLINE _r #-}

-- | Point of SECT193R1 curve
type P = BWPoint SECT193R1 F2m
