module Curve.ShortWeierstrass.SECP256K1
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

-- | SECP256K1 curve
data SECP256K1

-- | Field of SECP256K1 curve
type Fq = PrimeField 0xFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFEFFFFFC2F

-- | SECP256K1 curve is a short Weierstrass curve
instance SWCurve SECP256K1 Fq where
  _a _   = 0
  {-# INLINE _a #-}
  _b _   = 7
  {-# INLINE _b #-}
  _g     = notImplemented
  {-# INLINE _g #-}
  _h _ _ = 1
  {-# INLINE _h #-}
  _q _ _ = 0xFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFEFFFFFC2F
  {-# INLINE _q #-}
  _r _ _ = 0xFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFEBAAEDCE6AF48A03BBFD25E8CD0364141
  {-# INLINE _r #-}

-- | Point of SECP256K1 curve
type P = SWPoint SECP256K1 Fq
