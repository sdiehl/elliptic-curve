module Curve.ShortWeierstrass.SECP192K1
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

-- | SECP192K1 curve
data SECP192K1

-- | Field of SECP192K1 curve
type Fq = PrimeField 0xFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFEFFFFEE37

-- | SECP192K1 curve is a short Weierstrass curve
instance SWCurve SECP192K1 Fq where
  _a _   = 0
  {-# INLINE _a #-}
  _b _   = 3
  {-# INLINE _b #-}
  _g     = notImplemented
  {-# INLINE _g #-}
  _h _ _ = 1
  {-# INLINE _h #-}
  _q _ _ = 0xFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFEFFFFEE37
  {-# INLINE _q #-}
  _r _ _ = 0xFFFFFFFFFFFFFFFFFFFFFFFE26F2FC170F69466A74DEFD8D
  {-# INLINE _r #-}

-- | Point of SECP192K1 curve
type P = SWPoint SECP192K1 Fq
