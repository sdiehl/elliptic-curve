module Curve.ShortWeierstrass.SECP384R1
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

-- | SECP384R1 curve
data SECP384R1

-- | Field of SECP384R1 curve
type Fp = PrimeField 0xfffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffeffffffff0000000000000000ffffffff

-- | SECP384R1 curve is a short Weierstrass curve
instance SWCurve SECP384R1 Fp where
  _a _ = 0xfffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffeffffffff0000000000000000fffffffc
  {-# INLINE _a #-}
  _b _ = 0xb3312fa7e23ee7e4988e056be3f82d19181d9c6efe8141120314088f5013875ac656398d8a2ed19d2a85c8edd3ec2aef
  {-# INLINE _b #-}
  _g   = A 0xaa87ca22be8b05378eb1c71ef320ad746e1d3b628ba79b9859f741e082542a385502f25dbf55296c3a545e3872760ab7
           0x3617de4a96262c6f5d9e98bf9292dc29f8f41dbd289a147ce9da3113b5f0b8c00a60b1ce1d7e819d7a431d7c90ea0e5f
  {-# INLINE _g #-}
  _h _ = 1
  {-# INLINE _h #-}
  _n _ = 0xffffffffffffffffffffffffffffffffffffffffffffffffc7634d81f4372ddf581a0db248b0a77aecec196accc52973
  {-# INLINE _n #-}
  _p _ = 0xfffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffeffffffff0000000000000000ffffffff
  {-# INLINE _p #-}

-- | Point of SECP384R1 curve
type P = SWPoint SECP384R1 Fp
