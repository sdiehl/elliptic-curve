module Curve.ShortWeierstrass.BN128.G1
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

-- | BN128 curve G1
data G1

-- | Field of BN128 curve G1
type Fp = PrimeField 0x30644e72e131a029b85045b68181585d97816a916871ca8d3c208c16d87cfd47

-- | BN128 curve G1 is a short Weierstrass curve
instance SWCurve G1 Fp where
  _a _ = 0
  {-# INLINE _a #-}
  _b _ = 3
  {-# INLINE _b #-}
  _g   = A 1 2
  {-# INLINE _g #-}
  _h _ = 1
  {-# INLINE _h #-}
  _n _ = 0x30644e72e131a029b85045b68181585d2833e84879b9709143e1f593f0000001
  {-# INLINE _n #-}
  _p _ = 0x30644e72e131a029b85045b68181585d97816a916871ca8d3c208c16d87cfd47
  {-# INLINE _p #-}

-- | Point of BN128 curve G1
type P = SWPoint G1 Fp
