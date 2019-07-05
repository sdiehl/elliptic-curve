module Curve.BinaryWeierstrass.SECT409K1
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

-- | SECT409K1 curve
data SECT409K1

-- | Polynomial of SECT409K1 curve
data FX
instance IrreducibleMonic F2 FX where
  split _ = x ^ (409 :: Int) + x ^ (87 :: Int) + 1

-- | Field of SECT409K1 curve
type F2m = Fm FX

-- | SECT409K1 curve is a binary Weierstrass curve
instance BWCurve SECT409K1 FX where
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
  _r _ = 0x7FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFE5F83B2D4EA20400EC4557D5ED3E3E7CA5B4B5C83B8E01E5FCF
  {-# INLINE _r #-}

-- | Point of SECT409K1 curve
type P = BWPoint SECT409K1 F2m
