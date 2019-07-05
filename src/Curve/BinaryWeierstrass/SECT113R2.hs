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

-- | Field of SECT113R2 curve
data FX
instance IrreducibleMonic F2 FX where
  split _ = x ^ (113 :: Int) + x ^ (9 :: Int) + 1
type F2m = Fm FX

-- | SECT113R2 curve is a binary Weierstrass curve
instance BWCurve SECT113R2 FX where
  _a _ = 0x00689918dbec7e5a0dd6dfc0aa55c7
  {-# INLINE _a #-}
  _b _ = 0x0095e9a9ec9b297bd4bf36e059184f
  {-# INLINE _b #-}
  _f _ = witness :: FX
  {-# INLINE _f #-}
  _g   = A 0x01a57a6a7b26ca5ef52fcdb8164797
           0x00b3adc94ed1fe674c06e695baba1d
  {-# INLINE _g #-}
  _h _ = 2
  {-# INLINE _h #-}
  _n _ = 0x010000000000000108789b2496af93
  {-# INLINE _n #-}

-- | Point of SECT113R2 curve
type P = BWPoint SECT113R2 F2m
