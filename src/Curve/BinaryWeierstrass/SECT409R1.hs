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

-- | Polynomial of SECT409R1 curve
data FX
instance IrreducibleMonic F2 FX where
  split _ = x ^ (409 :: Int) + x ^ (87 :: Int) + 1

-- | Field of SECT409R1 curve
type F2m = Fm FX

-- | SECT409R1 curve is a binary Weierstrass curve
instance BWCurve SECT409R1 FX where
  _a _ = 1
  {-# INLINE _a #-}
  _b _ = 0x0021A5C2C8EE9FEB5C4B9A753B7B476B7FD6422EF1F3DD674761FA99D6AC27C8A9A197B272822F6CD57A55AA4F50AE317B13545F
  {-# INLINE _b #-}
  _f _ = witness :: FX
  {-# INLINE _f #-}
  _g   = notImplemented
  {-# INLINE _g #-}
  _h _ = 2
  {-# INLINE _h #-}
  _r _ = 0x010000000000000000000000000000000000000000000000000001E2AAD6A612F33307BE5FA47C3C9E052F838164CD37D9A21173
  {-# INLINE _r #-}

-- | Point of SECT409R1 curve
type P = BWPoint SECT409R1 F2m
