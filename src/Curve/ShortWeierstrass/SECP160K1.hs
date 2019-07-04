module Curve.ShortWeierstrass.SECP160K1
  -- | Imports
  ( Point(..)
  , SWCurve(..)
  , SWPoint
  -- | Types
  , Fq
  , P
  ) where

import Protolude

import PrimeField (PrimeField)

import Curve.ShortWeierstrass (Point(..), SWCurve(..), SWPoint)

-------------------------------------------------------------------------------
-- Types
-------------------------------------------------------------------------------

-- | SECP160K1 curve
data SECP160K1

-- | Field of SECP160K1 curve
type Fq = PrimeField 0xFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFEFFFFAC73

-- | SECP160K1 curve is a short Weierstrass curve
instance SWCurve SECP160K1 Fq where
  _a _   = 0
  {-# INLINE _a #-}
  _b _   = 7
  {-# INLINE _b #-}
  _g     = notImplemented
  {-# INLINE _g #-}
  _h _ _ = 1
  {-# INLINE _h #-}
  _q _ _ = 0xFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFEFFFFAC73
  {-# INLINE _q #-}
  _r _ _ = 0x0100000000000000000001B8FA16DFAB9ACA16B6B3
  {-# INLINE _r #-}

-- | Point of SECP160K1 curve
type P = SWPoint SECP160K1 Fq
