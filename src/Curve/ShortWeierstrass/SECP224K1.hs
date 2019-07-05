module Curve.ShortWeierstrass.SECP224K1
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

-- | SECP224K1 curve
data SECP224K1

-- | Field of SECP224K1 curve
type Fq = PrimeField 0xFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFEFFFFE56D

-- | SECP224K1 curve is a short Weierstrass curve
instance SWCurve SECP224K1 Fq where
  _a _ = 0
  {-# INLINE _a #-}
  _b _ = 5
  {-# INLINE _b #-}
  _g   = notImplemented
  {-# INLINE _g #-}
  _h _ = 1
  {-# INLINE _h #-}
  _q _ = 0xFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFEFFFFE56D
  {-# INLINE _q #-}
  _r _ = 0x010000000000000000000000000001DCE8D2EC6184CAF0A971769FB1F7
  {-# INLINE _r #-}

-- | Point of SECP224K1 curve
type P = SWPoint SECP224K1 Fq
