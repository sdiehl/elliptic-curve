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

-- | Polynomial of SECT193R2 curve
data FX
instance IrreducibleMonic F2 FX where
  split _ = x ^ (193 :: Int) + x ^ (15 :: Int) + 1

-- | Field of SECT193R2 curve
type F2m = Fm FX

-- | SECT193R2 curve is a binary Weierstrass curve
instance BWCurve SECT193R2 FX where
  _a _ = 0x0163F35A5137C2CE3EA6ED8667190B0BC43ECD69977702709B
  {-# INLINE _a #-}
  _b _ = 0x00C9BB9E8927D4D64C377E2AB2856A5B16E3EFB7F61D4316AE
  {-# INLINE _b #-}
  _f _ = witness :: FX
  {-# INLINE _f #-}
  _g   = notImplemented
  {-# INLINE _g #-}
  _h _ = 2
  {-# INLINE _h #-}
  _r _ = 0x010000000000000000000000015AAB561B005413CCD4EE99D5
  {-# INLINE _r #-}

-- | Point of SECT193R2 curve
type P = BWPoint SECT193R2 F2m
