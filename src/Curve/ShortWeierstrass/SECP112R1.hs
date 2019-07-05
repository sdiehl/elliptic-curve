module Curve.ShortWeierstrass.SECP112R1
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

-- | SECP112R1 curve
data SECP112R1

-- | Field of SECP112R1 curve
type Fp = PrimeField 0xdb7c2abf62e35e668076bead208b

-- | SECP112R1 curve is a short Weierstrass curve
instance SWCurve SECP112R1 Fp where
  _a _ = 0xdb7c2abf62e35e668076bead2088
  {-# INLINE _a #-}
  _b _ = 0x659ef8ba043916eede8911702b22
  {-# INLINE _b #-}
  _g   = A 0x09487239995a5ee76b55f9c2f098
           0xa89ce5af8724c0a23e0e0ff77500
  {-# INLINE _g #-}
  _h _ = 1
  {-# INLINE _h #-}
  _n _ = 0xdb7c2abf62e35e7628dfac6561c5
  {-# INLINE _n #-}
  _p _ = 0xdb7c2abf62e35e668076bead208b
  {-# INLINE _p #-}

-- | Point of SECP112R1 curve
type P = SWPoint SECP112R1 Fp
