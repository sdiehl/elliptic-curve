module Curve.ShortWeierstrass.SECP160R2
  -- | Imports
  ( Point(..)
  , SWCurve(..)
  , SWPoint
  -- | Types
  , Fp
  , P
  ) where

import PrimeField (PrimeField)

import Curve.ShortWeierstrass (Point(..), SWCurve(..), SWPoint)

-------------------------------------------------------------------------------
-- Types
-------------------------------------------------------------------------------

-- | SECP160R2 curve
data SECP160R2

-- | Field of SECP160R2 curve
type Fp = PrimeField 0xfffffffffffffffffffffffffffffffeffffac73

-- | SECP160R2 curve is a short Weierstrass curve
instance SWCurve SECP160R2 Fp where
  _a _ = 0xfffffffffffffffffffffffffffffffeffffac70
  {-# INLINE _a #-}
  _b _ = 0xb4e134d3fb59eb8bab57274904664d5af50388ba
  {-# INLINE _b #-}
  _g   = A 0x0052dcb034293a117e1f4ff11b30f7199d3144ce6d
           0x00feaffef2e331f296e071fa0df9982cfea7d43f2e
  {-# INLINE _g #-}
  _h _ = 4
  {-# INLINE _h #-}
  _n _ = 0x0100000000000000000000351ee786a818f3a1a16b
  {-# INLINE _n #-}
  _p _ = 0xfffffffffffffffffffffffffffffffeffffac73
  {-# INLINE _p #-}

-- | Point of SECP160R2 curve
type P = SWPoint SECP160R2 Fp
