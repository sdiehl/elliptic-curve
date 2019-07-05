module Curve.ShortWeierstrass.SECP256R1
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

-- | SECP256R1 curve
data SECP256R1

-- | Field of SECP256R1 curve
type Fp = PrimeField 0xffffffff00000001000000000000000000000000ffffffffffffffffffffffff

-- | SECP256R1 curve is a short Weierstrass curve
instance SWCurve SECP256R1 Fp where
  _a _ = 0xffffffff00000001000000000000000000000000fffffffffffffffffffffffc
  {-# INLINE _a #-}
  _b _ = 0x5ac635d8aa3a93e7b3ebbd55769886bc651d06b0cc53b0f63bce3c3e27d2604b
  {-# INLINE _b #-}
  _g   = A 0x6b17d1f2e12c4247f8bce6e563a440f277037d812deb33a0f4a13945d898c296
           0x4fe342e2fe1a7f9b8ee7eb4a7c0f9e162bce33576b315ececbb6406837bf51f5
  {-# INLINE _g #-}
  _h _ = 1
  {-# INLINE _h #-}
  _n _ = 0xffffffff00000000ffffffffffffffffbce6faada7179e84f3b9cac2fc632551
  {-# INLINE _n #-}
  _p _ = 0xffffffff00000001000000000000000000000000ffffffffffffffffffffffff
  {-# INLINE _p #-}

-- | Point of SECP256R1 curve
type P = SWPoint SECP256R1 Fp
