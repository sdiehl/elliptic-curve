module Curve.BinaryWeierstrass.SECT193R2
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

-- | SECT193R2 curve
data SECT193R2

-- | Field of SECT193R2 curve
data FX
instance IrreducibleMonic F2 FX where
  split _ = x ^ (193 :: Int) + x ^ (15 :: Int) + 1
type F2m = Fm FX

-- | SECT193R2 curve is a binary Weierstrass curve
instance BWCurve SECT193R2 FX where
  _a _ = 0x0163f35a5137c2ce3ea6ed8667190b0bc43ecd69977702709b
  {-# INLINE _a #-}
  _b _ = 0x00c9bb9e8927d4d64c377e2ab2856a5b16e3efb7f61d4316ae
  {-# INLINE _b #-}
  _f _ = witness :: FX
  {-# INLINE _f #-}
  _g   = A 0x00d9b67d192e0367c803f39e1a7e82ca14a651350aae617e8f
           0x01ce94335607c304ac29e7defbd9ca01f596f927224cdecf6c
  {-# INLINE _g #-}
  _h _ = 2
  {-# INLINE _h #-}
  _n _ = 0x010000000000000000000000015aab561b005413ccd4ee99d5
  {-# INLINE _n #-}

-- | Point of SECT193R2 curve
type P = BWPoint SECT193R2 F2m
