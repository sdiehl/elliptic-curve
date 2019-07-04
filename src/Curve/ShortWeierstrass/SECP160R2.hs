module Curve.ShortWeierstrass.SECP160R2
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

-- | SECP160R2 curve
data SECP160R2

-- | Field of SECP160R2 curve
type Fq = PrimeField 0xFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFEFFFFAC73

-- | SECP160R2 curve is a short Weierstrass curve
instance SWCurve SECP160R2 Fq where
  _a _   = 0xFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFEFFFFAC70
  {-# INLINE _a #-}
  _b _   = 0xB4E134D3FB59EB8BAB57274904664D5AF50388BA
  {-# INLINE _b #-}
  _g     = notImplemented
  {-# INLINE _g #-}
  _h _ _ = 4
  {-# INLINE _h #-}
  _q _ _ = 0xFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFEFFFFAC73
  {-# INLINE _q #-}
  _r _ _ = 0x0100000000000000000000351EE786A818F3A1A16B
  {-# INLINE _r #-}

-- | Point of SECP160R2 curve
type P = SWPoint SECP160R2 Fq
