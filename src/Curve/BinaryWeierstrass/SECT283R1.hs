module Curve.BinaryWeierstrass.SECT283R1
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

-- | SECT283R1 curve
data SECT283R1

-- | Polynomial of SECT283R1 curve
data FX
instance IrreducibleMonic F2 FX where
  split _ = x ^ (283 :: Int) + x ^ (12 :: Int) + x ^ (7 :: Int) + x ^ (5 :: Int) + 1

-- | Field of SECT283R1 curve
type F2m = Fm FX

-- | SECT283R1 curve is a binary Weierstrass curve
instance BWCurve SECT283R1 FX where
  _a _ = 1
  {-# INLINE _a #-}
  _b _ = 0x027B680AC8B8596DA5A4AF8A19A0303FCA97FD7645309FA2A581485AF6263E313B79A2F5
  {-# INLINE _b #-}
  _f _ = witness :: FX
  {-# INLINE _f #-}
  _g   = notImplemented
  {-# INLINE _g #-}
  _h _ = 2
  {-# INLINE _h #-}
  _r _ = 0x03FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFEF90399660FC938A90165B042A7CEFADB307
  {-# INLINE _r #-}

-- | Point of SECT283R1 curve
type P = BWPoint SECT283R1 F2m
