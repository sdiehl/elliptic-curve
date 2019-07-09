module Curve.TwistedEdwards.JubJub
  -- | Types
  ( Fp
  , P
  -- | Parameters
  , _a
  , _d
  , _g
  , _h
  , _n
  , _p
  ) where

import Protolude

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
  a_ = const _a
  {-# INLINE a_ #-}
  d_ = const _d
  {-# INLINE d_ #-}
  g_ = _g
  {-# INLINE g_ #-}

-- | Point of JubJub curve
type P = TEPoint JubJub Fp

-------------------------------------------------------------------------------
-- Parameters
-------------------------------------------------------------------------------

-- | Coefficient @A@ of JubJub curve
_a :: Fp
_a = -1
{-# INLINE _a #-}

-- | Coefficient @D@ of JubJub curve
_d :: Fp
_d = 0x2a9318e74bfa2b48f5fd9207e6bd7fd4292d7f6d37579d2601065fd6d6343eb1
{-# INLINE _d #-}

-- | Generator of JubJub curve
_g :: P
_g = A
     3
     0x39d39d6e76c20811ee2fb44b2286fd27c0d43e2dfb6f72af672e03db8d560cb5
{-# INLINE _g #-}

-- | Cofactor of JubJub curve
_h :: Integer
_h = 8
{-# INLINE _h #-}

-- | Order of JubJub curve
_n :: Integer
_n = 0x0e7db4ea6533afa906673b0101343b00a6682093ccc81082d0970e5ed6f72cb7
{-# INLINE _n #-}

-- | Characteristic of JubJub curve
_p :: Integer
_p = 0x73eda753299d7d483339d80809a1d80553bda402fffe5bfeffffffff00000001
{-# INLINE _p #-}
