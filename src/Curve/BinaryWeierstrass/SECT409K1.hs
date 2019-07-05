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

-- | Field of SECT409K1 curve
data FX
instance IrreducibleMonic F2 FX where
  split _ = x ^ (409 :: Int) + x ^ (87 :: Int) + 1
type F2m = Fm FX

-- | SECT409K1 curve is a binary Weierstrass curve
instance BWCurve SECT409K1 FX where
  _a _ = 0
  {-# INLINE _a #-}
  _b _ = 1
  {-# INLINE _b #-}
  _f _ = witness :: FX
  {-# INLINE _f #-}
  _g   = A 0x0060f05f658f49c1ad3ab1890f7184210efd0987e307c84c27accfb8f9f67cc2c460189eb5aaaa62ee222eb1b35540cfe9023746
           0x01e369050b7c4e42acba1dacbf04299c3460782f918ea427e6325165e9ea10e3da5f6c42e9c55215aa9ca27a5863ec48d8e0286b
  {-# INLINE _g #-}
  _h _ = 4
  {-# INLINE _h #-}
  _n _ = 0x7ffffffffffffffffffffffffffffffffffffffffffffffffffe5f83b2d4ea20400ec4557d5ed3e3e7ca5b4b5c83b8e01e5fcf
  {-# INLINE _n #-}

-- | Point of SECT409K1 curve
type P = BWPoint SECT409K1 F2m
