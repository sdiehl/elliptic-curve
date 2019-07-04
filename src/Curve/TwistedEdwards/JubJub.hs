module Curve.TwistedEdwards.JubJub
  -- | Imports
  ( Point(..)
  , TECurve(..)
  , TEPoint
  -- | Types
  , Fq
  , P
  -- | Parameters
  , _d
  , _h
  , _q
  , _r
  ) where

import Protolude

import PrimeField (PrimeField)

import Curve.TwistedEdwards (Point(..), TECurve(..), TEPoint)

-------------------------------------------------------------------------------
-- Types
-------------------------------------------------------------------------------

-- | JubJub curve @-x^2+y^2=1+dx^2y^2@
data JubJub

-- | Field of JubJub curve
type Fq = PrimeField 0x73eda753299d7d483339d80809a1d80553bda402fffe5bfeffffffff00000001

-- | JubJub curve is a twisted Edwards curve
instance TECurve JubJub Fq where
  a _ = -1
  d _ = _d

-- | Point of JubJub curve
type P = TEPoint JubJub Fq

-------------------------------------------------------------------------------
-- Parameters
-------------------------------------------------------------------------------

-- | Coefficient of JubJub curve
_d :: Fq
_d = -(10240 / 10241)
{-# INLINE _d #-}

-- | Cofactor of JubJub curve
_h :: Integer
_h = 8
{-# INLINE _h #-}

-- | Characteristic of JubJub curve
_q :: Fq
_q = 0x73eda753299d7d483339d80809a1d80553bda402fffe5bfeffffffff00000001
{-# INLINE _q #-}

-- | Order of JubJub curve
_r :: Integer
_r = 0x0e7db4ea6533afa906673b0101343b00a6682093ccc81082d0970e5ed6f72cb7
{-# INLINE _r #-}
