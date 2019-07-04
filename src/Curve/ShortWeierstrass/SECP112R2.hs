module Curve.ShortWeierstrass.SECP112R2
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

-- | SECP112R2 curve
data SECP112R2

-- | Field of SECP112R2 curve
type Fq = PrimeField 0xDB7C2ABF62E35E668076BEAD208B

-- | SECP112R2 curve is a short Weierstrass curve
instance SWCurve SECP112R2 Fq where
  _a _   = 0x6127C24C05F38A0AAAF65C0EF02C
  {-# INLINE _a #-}
  _b _   = 0x51DEF1815DB5ED74FCC34C85D709
  {-# INLINE _b #-}
  _g     = notImplemented
  {-# INLINE _g #-}
  _h _ _ = 4
  {-# INLINE _h #-}
  _q _ _ = 0xDB7C2ABF62E35E668076BEAD208B
  {-# INLINE _q #-}
  _r _ _ = 0x36DF0AAFD8B8D7597CA10520D04B
  {-# INLINE _r #-}

-- | Point of SECP112R2 curve
type P = SWPoint SECP112R2 Fq
