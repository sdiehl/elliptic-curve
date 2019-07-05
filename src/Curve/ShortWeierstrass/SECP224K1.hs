module Curve.ShortWeierstrass.SECP224K1
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

-- | SECP224K1 curve
data SECP224K1

-- | Field of SECP224K1 curve
type Fp = PrimeField 0xfffffffffffffffffffffffffffffffffffffffffffffffeffffe56d

-- | SECP224K1 curve is a short Weierstrass curve
instance SWCurve SECP224K1 Fp where
  _a _ = 0
  {-# INLINE _a #-}
  _b _ = 5
  {-# INLINE _b #-}
  _g   = A 0x00a1455b334df099df30fc28a169a467e9e47075a90f7e650eb6b7a45c
           0x007e089fed7fba344282cafbd6f7e319f7c0b0bd59e2ca4bdb556d61a5
  {-# INLINE _g #-}
  _h _ = 1
  {-# INLINE _h #-}
  _n _ = 0x010000000000000000000000000001dce8d2ec6184caf0a971769fb1f7
  {-# INLINE _n #-}
  _p _ = 0xfffffffffffffffffffffffffffffffffffffffffffffffeffffe56d
  {-# INLINE _p #-}

-- | Point of SECP224K1 curve
type P = SWPoint SECP224K1 Fp
