module Curve.TwistedEdwards.JubJub
  -- | Imports
  ( Point(..)
  , TECurve(..)
  , TEPoint
  -- | Types
  , Fq
  , P
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
type Fq = PrimeField 0x73eda753299d7d483339d80809a1d80553bda402fffe5bfeffffffff00000001

-- | JubJub curve is a twisted Edwards curve
instance TECurve JubJub Fq where
  _a _   = -1
  {-# INLINE _a #-}
  _d _   = -(10240 / 10241)
  {-# INLINE _d #-}
  _g     = notImplemented
  {-# INLINE _g #-}
  _h _ _ = 8
  {-# INLINE _h #-}
  _q _ _ = 0x73eda753299d7d483339d80809a1d80553bda402fffe5bfeffffffff00000001
  {-# INLINE _q #-}
  _r _ _ = 0x0e7db4ea6533afa906673b0101343b00a6682093ccc81082d0970e5ed6f72cb7
  {-# INLINE _r #-}

-- | Point of JubJub curve
type P = TEPoint JubJub Fq
