module Curve.ShortWeierstrass.SECP160R1
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

-- | SECP160R1 curve
data SECP160R1

-- | Field of SECP160R1 curve
type Fq = PrimeField 0xFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF7FFFFFFF

-- | SECP160R1 curve is a short Weierstrass curve
instance SWCurve SECP160R1 Fq where
  _a _   = 0xFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF7FFFFFFC
  {-# INLINE _a #-}
  _b _   = 0x1C97BEFC54BD7A8B65ACF89F81D4D4ADC565FA45
  {-# INLINE _b #-}
  _g     = notImplemented
  {-# INLINE _g #-}
  _h _ _ = 1
  {-# INLINE _h #-}
  _q _ _ = 0xFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF7FFFFFFF
  {-# INLINE _q #-}
  _r _ _ = 0x0100000000000000000001F4C8F927AED3CA752257
  {-# INLINE _r #-}

-- | Point of SECP160R1 curve
type P = SWPoint SECP160R1 Fq
