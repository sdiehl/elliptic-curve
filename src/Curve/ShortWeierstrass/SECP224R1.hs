module Curve.ShortWeierstrass.SECP224R1
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

-- | SECP224R1 curve
data SECP224R1

-- | Field of SECP224R1 curve
type Fp = PrimeField 0xffffffffffffffffffffffffffffffff000000000000000000000001

-- | SECP224R1 curve is a short Weierstrass curve
instance SWCurve SECP224R1 Fp where
  _a _ = 0xfffffffffffffffffffffffffffffffefffffffffffffffffffffffe
  {-# INLINE _a #-}
  _b _ = 0xb4050a850c04b3abf54132565044b0b7d7bfd8ba270b39432355ffb4
  {-# INLINE _b #-}
  _g   = A 0xb70e0cbd6bb4bf7f321390b94a03c1d356c21122343280d6115c1d21
           0xbd376388b5f723fb4c22dfe6cd4375a05a07476444d5819985007e34
  {-# INLINE _g #-}
  _h _ = 1 
  {-# INLINE _h #-}
  _n _ = 0xffffffffffffffffffffffffffff16a2e0b8f03e13dd29455c5c2a3d
  {-# INLINE _n #-}
  _p _ = 0xffffffffffffffffffffffffffffffff000000000000000000000001
  {-# INLINE _p #-}

-- | Point of SECP224R1 curve
type P = SWPoint SECP224R1 Fp
