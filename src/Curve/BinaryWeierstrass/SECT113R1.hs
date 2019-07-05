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

-- | Polynomial of SECT113R1 curve
data FX
instance IrreducibleMonic F2 FX where
  split _ = x ^ (113 :: Int) + x ^ (9 :: Int) + 1

-- | Field of SECT113R1 curve
type F2m = Fm FX

-- | SECT113R1 curve is a binary Weierstrass curve
instance BWCurve SECT113R1 FX where
  _a _ = 0x003088250CA6E7C7FE649CE85820F7
  {-# INLINE _a #-}
  _b _ = 0x00E8BEE4D3E2260744188BE0E9C723
  {-# INLINE _b #-}
  _f _ = witness :: FX
  {-# INLINE _f #-}
  _g   = notImplemented
  {-# INLINE _g #-}
  _h _ = 2
  {-# INLINE _h #-}
  _r _ = 0x0100000000000000D9CCEC8A39E56F
  {-# INLINE _r #-}

-- | Point of SECT113R1 curve
type P = BWPoint SECT113R1 F2m
