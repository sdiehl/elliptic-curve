module Curve.TwistedEdwards.JubJub
  -- | Imports
  ( Point(..)
  , TECurve(..)
  , TEPoint
  -- | Types
  , Fp
  , P
  ) where

import PrimeField (PrimeField)

import Curve.TwistedEdwards (Point(..), TECurve(..), TEPoint)

-------------------------------------------------------------------------------
-- Types
-------------------------------------------------------------------------------

-- | JubJub curve
data JubJub

-- | Field of JubJub curve
type Fp = PrimeField 0x73eda753299d7d483339d80809a1d80553bda402fffe5bfeffffffff00000001

-- | JubJub curve is a twisted Edwards curve
instance TECurve JubJub Fp where
  _a _ = -1
  {-# INLINE _a #-}
  _d _ = 0x2a9318e74bfa2b48f5fd9207e6bd7fd4292d7f6d37579d2601065fd6d6343eb1
  {-# INLINE _d #-}
  _g   = A 3
           0x39d39d6e76c20811ee2fb44b2286fd27c0d43e2dfb6f72af672e03db8d560cb5
  {-# INLINE _g #-}
  _h _ = 8
  {-# INLINE _h #-}
  _n _ = 0x0e7db4ea6533afa906673b0101343b00a6682093ccc81082d0970e5ed6f72cb7
  {-# INLINE _n #-}
  _p _ = 0x73eda753299d7d483339d80809a1d80553bda402fffe5bfeffffffff00000001
  {-# INLINE _p #-}

-- | Point of JubJub curve
type P = TEPoint JubJub Fp
