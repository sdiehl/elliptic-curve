module Curve.ShortWeierstrass.SECP160R1
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

-- | SECP160R1 curve
data SECP160R1

-- | Field of SECP160R1 curve
type Fp = PrimeField 0xffffffffffffffffffffffffffffffff7fffffff

-- | SECP160R1 curve is a short Weierstrass curve
instance SWCurve SECP160R1 Fp where
  _a _ = 0xffffffffffffffffffffffffffffffff7ffffffc
  {-# INLINE _a #-}
  _b _ = 0x1c97befc54bd7a8b65acf89f81d4d4adc565fa45
  {-# INLINE _b #-}
  _g   = A 0x004a96b5688ef573284664698968c38bb913cbfc82
           0x0023a628553168947d59dcc912042351377ac5fb32
  {-# INLINE _g #-}
  _h _ = 1
  {-# INLINE _h #-}
  _n _ = 0x0100000000000000000001f4c8f927aed3ca752257
  {-# INLINE _n #-}
  _p _ = 0xffffffffffffffffffffffffffffffff7fffffff
  {-# INLINE _p #-}

-- | Point of SECP160R1 curve
type P = SWPoint SECP160R1 Fp
