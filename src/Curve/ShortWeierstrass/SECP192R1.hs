module Curve.ShortWeierstrass.SECP192R1
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

-- | SECP192R1 curve
data SECP192R1

-- | Field of SECP192R1 curve
type Fq = PrimeField 0xFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFEFFFFFFFFFFFFFFFF

-- | SECP192R1 curve is a short Weierstrass curve
instance SWCurve SECP192R1 Fq where
  _a _ = 0xFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFEFFFFFFFFFFFFFFFC
  {-# INLINE _a #-}
  _b _ = 0x64210519E59C80E70FA7E9AB72243049FEB8DEECC146B9B1
  {-# INLINE _b #-}
  _g   = notImplemented
  {-# INLINE _g #-}
  _h _ = 1
  {-# INLINE _h #-}
  _q _ = 0xFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFEFFFFFFFFFFFFFFFF
  {-# INLINE _q #-}
  _r _ = 0xFFFFFFFFFFFFFFFFFFFFFFFF99DEF836146BC9B1B4D22831
  {-# INLINE _r #-}

-- | Point of SECP192R1 curve
type P = SWPoint SECP192R1 Fq
