module Curve.BinaryWeierstrass.SECT233R1
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

-- | SECT233R1 curve
data SECT233R1

-- | Polynomial of SECT233R1 curve
data FX
instance IrreducibleMonic F2 FX where
  split _ = x ^ (233 :: Int) + x ^ (74 :: Int) + 1

-- | Field of SECT233R1 curve
type F2m = Fm FX

-- | SECT233R1 curve is a binary Weierstrass curve
instance BWCurve SECT233R1 FX where
  _a _ = 1
  {-# INLINE _a #-}
  _b _ = 0x0066647EDE6C332C7F8C0923BB58213B333B20E9CE4281FE115F7D8F90AD
  {-# INLINE _b #-}
  _f _ = witness :: FX
  {-# INLINE _f #-}
  _g   = notImplemented
  {-# INLINE _g #-}
  _h _ = 2
  {-# INLINE _h #-}
  _r _ = 0x01000000000000000000000000000013E974E72F8A6922031D2603CFE0D7
  {-# INLINE _r #-}

-- | Point of SECT233R1 curve
type P = BWPoint SECT233R1 F2m
