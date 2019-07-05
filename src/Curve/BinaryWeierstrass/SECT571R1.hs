module Curve.BinaryWeierstrass.SECT571R1
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

-- | SECT571R1 curve
data SECT571R1

-- | Polynomial of SECT571R1 curve
data FX
instance IrreducibleMonic F2 FX where
  split _ = x ^ (571 :: Int) + x ^ (10 :: Int) + x ^ (5 :: Int) + x ^ (2 :: Int) + 1

-- | Field of SECT571R1 curve
type F2m = Fm FX

-- | SECT571R1 curve is a binary Weierstrass curve
instance BWCurve SECT571R1 FX where
  _a _ = 1
  {-# INLINE _a #-}
  _b _ = 0x02F40E7E2221F295DE297117B7F3D62F5C6A97FFCB8CEFF1CD6BA8CE4A9A18AD84FFABBD8EFA59332BE7AD6756A66E294AFD185A78FF12AA520E4DE739BACA0C7FFEFF7F2955727A
  {-# INLINE _b #-}
  _f _ = witness :: FX
  {-# INLINE _f #-}
  _g   = notImplemented
  {-# INLINE _g #-}
  _h _ = 2
  {-# INLINE _h #-}
  _r _ = 0x03FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFE661CE18FF55987308059B186823851EC7DD9CA1161DE93D5174D66E8382E9BB2FE84E47
  {-# INLINE _r #-}

-- | Point of SECT571R1 curve
type P = BWPoint SECT571R1 F2m
