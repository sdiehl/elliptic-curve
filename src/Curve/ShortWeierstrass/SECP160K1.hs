module Curve.ShortWeierstrass.SECP160K1
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

-- | SECP160K1 curve
data SECP160K1

-- | Field of SECP160K1 curve
type Fp = PrimeField 0xfffffffffffffffffffffffffffffffeffffac73

-- | SECP160K1 curve is a short Weierstrass curve
instance SWCurve SECP160K1 Fp where
  _a _ = 0
  {-# INLINE _a #-}
  _b _ = 7
  {-# INLINE _b #-}
  _g   = A 0x003b4c382ce37aa192a4019e763036f4f5dd4d7ebb
           0x00938cf935318fdced6bc28286531733c3f03c4fee
  {-# INLINE _g #-}
  _h _ = 1
  {-# INLINE _h #-}
  _n _ = 0x0100000000000000000001b8fa16dfab9aca16b6b3
  {-# INLINE _n #-}
  _p _ = 0xfffffffffffffffffffffffffffffffeffffac73
  {-# INLINE _p #-}

-- | Point of SECP160K1 curve
type P = SWPoint SECP160K1 Fp
