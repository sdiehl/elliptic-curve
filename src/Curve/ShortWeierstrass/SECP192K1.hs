module Curve.ShortWeierstrass.SECP192K1
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

-- | SECP192K1 curve
data SECP192K1

-- | Field of SECP192K1 curve
type Fp = PrimeField 0xfffffffffffffffffffffffffffffffffffffffeffffee37

-- | SECP192K1 curve is a short Weierstrass curve
instance SWCurve SECP192K1 Fp where
  _a _ = 0
  {-# INLINE _a #-}
  _b _ = 3
  {-# INLINE _b #-}
  _g   = A 0xdb4ff10ec057e9ae26b07d0280b7f4341da5d1b1eae06c7d
           0x9b2f2f6d9c5628a7844163d015be86344082aa88d95e2f9d
  {-# INLINE _g #-}
  _h _ = 1
  {-# INLINE _h #-}
  _n _ = 0xfffffffffffffffffffffffe26f2fc170f69466a74defd8d
  {-# INLINE _n #-}
  _p _ = 0xfffffffffffffffffffffffffffffffffffffffeffffee37
  {-# INLINE _p #-}

-- | Point of SECP192K1 curve
type P = SWPoint SECP192K1 Fp
