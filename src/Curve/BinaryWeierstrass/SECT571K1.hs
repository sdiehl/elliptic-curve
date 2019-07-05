module Curve.BinaryWeierstrass.SECT571K1
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

-- | SECT571K1 curve
data SECT571K1

-- | Polynomial of SECT571K1 curve
data FX
instance IrreducibleMonic F2 FX where
  split _ = x ^ (571 :: Int) + x ^ (10 :: Int) + x ^ (5 :: Int) + x ^ (2 :: Int) + 1

-- | Field of SECT571K1 curve
type F2m = Fm FX

-- | SECT571K1 curve is a binary Weierstrass curve
instance BWCurve SECT571K1 FX where
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
  _r _ = 0x020000000000000000000000000000000000000000000000000000000000000000000000131850E1F19A63E4B391A8DB917F4138B630D84BE5D639381E91DEB45CFE778F637C1001
  {-# INLINE _r #-}

-- | Point of SECT571K1 curve
type P = BWPoint SECT571K1 F2m
