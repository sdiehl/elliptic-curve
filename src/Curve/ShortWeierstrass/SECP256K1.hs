module Curve.ShortWeierstrass.SECP256K1
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

-- | SECP256K1 curve
data SECP256K1

-- | Field of SECP256K1 curve
type Fp = PrimeField 0xfffffffffffffffffffffffffffffffffffffffffffffffffffffffefffffc2f

-- | SECP256K1 curve is a short Weierstrass curve
instance SWCurve SECP256K1 Fp where
  _a _ = 0
  {-# INLINE _a #-}
  _b _ = 7
  {-# INLINE _b #-}
  _g   = A 0x79be667ef9dcbbac55a06295ce870b07029bfcdb2dce28d959f2815b16f81798
           0x483ada7726a3c4655da4fbfc0e1108a8fd17b448a68554199c47d08ffb10d4b8
  {-# INLINE _g #-}
  _h _ = 1
  {-# INLINE _h #-}
  _n _ = 0xfffffffffffffffffffffffffffffffebaaedce6af48a03bbfd25e8cd0364141
  {-# INLINE _n #-}
  _p _ = 0xfffffffffffffffffffffffffffffffffffffffffffffffffffffffefffffc2f
  {-# INLINE _p #-}

-- | Point of SECP256K1 curve
type P = SWPoint SECP256K1 Fp
