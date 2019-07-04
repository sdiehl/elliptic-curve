module Curve.ShortWeierstrass.SECP256R1
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

-- | SECP256R1 curve
data SECP256R1

-- | Field of SECP256R1 curve
type Fq = PrimeField 0xFFFFFFFF00000001000000000000000000000000FFFFFFFFFFFFFFFFFFFFFFFF

-- | SECP256R1 curve is a short Weierstrass curve
instance SWCurve SECP256R1 Fq where
  _a _   = 0xFFFFFFFF00000001000000000000000000000000FFFFFFFFFFFFFFFFFFFFFFFC
  {-# INLINE _a #-}
  _b _   = 0x5AC635D8AA3A93E7B3EBBD55769886BC651D06B0CC53B0F63BCE3C3E27D2604B
  {-# INLINE _b #-}
  _g     = notImplemented
  {-# INLINE _g #-}
  _h _ _ = 1
  {-# INLINE _h #-}
  _q _ _ = 0xFFFFFFFF00000001000000000000000000000000FFFFFFFFFFFFFFFFFFFFFFFF
  {-# INLINE _q #-}
  _r _ _ = 0xFFFFFFFF00000000FFFFFFFFFFFFFFFFBCE6FAADA7179E84F3B9CAC2FC632551
  {-# INLINE _r #-}

-- | Point of SECP256R1 curve
type P = SWPoint SECP256R1 Fq
