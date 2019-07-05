module Curve.ShortWeierstrass.SECP128R1
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

-- | SECP128R1 curve
data SECP128R1

-- | Field of SECP128R1 curve
type Fp = PrimeField 0xfffffffdffffffffffffffffffffffff

-- | SECP128R1 curve is a short Weierstrass curve
instance SWCurve SECP128R1 Fp where
  _a _ = 0xfffffffdfffffffffffffffffffffffc
  {-# INLINE _a #-}
  _b _ = 0xe87579c11079f43dd824993c2cee5ed3
  {-# INLINE _b #-}
  _g   = A 0x161ff7528b899b2d0c28607ca52c5b86
           0xcf5ac8395bafeb13c02da292dded7a83
  {-# INLINE _g #-}
  _h _ = 1
  {-# INLINE _h #-}
  _n _ = 0xfffffffe0000000075a30d1b9038a115
  {-# INLINE _n #-}
  _p _ = 0xfffffffdffffffffffffffffffffffff
  {-# INLINE _p #-}

-- | Point of SECP128R1 curve
type P = SWPoint SECP128R1 Fp
