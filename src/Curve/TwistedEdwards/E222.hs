module Curve.TwistedEdwards.E222
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

-- | E-222 curve
data E222

-- | Field of E-222 curve
type Fp = PrimeField 0x3fffffffffffffffffffffffffffffffffffffffffffffffffffff8b

-- | E-222 curve is a twisted Edwards curve
instance TECurve E222 Fp where
  a_ = const _a
  {-# INLINE a_ #-}
  d_ = const _d
  {-# INLINE d_ #-}
  g_ = _g
  {-# INLINE g_ #-}

-- | Point of E-222 curve
type P = TEPoint E222 Fp

-------------------------------------------------------------------------------
-- Parameters
-------------------------------------------------------------------------------

-- | Coefficient @A@ of E-222 curve
_a :: Fp
_a = 1
{-# INLINE _a #-}

-- | Coefficient @D@ of E-222 curve
_d :: Fp
_d = 0x27166
{-# INLINE _d #-}

-- | Generator of E-222 curve
_g :: P
_g = A
     0x19b12bb156a389e55c9768c303316d07c23adab3736eb2bc3eb54e51
     0x1c
{-# INLINE _g #-}

-- | Cofactor of E-222 curve
_h :: Integer
_h = 4
{-# INLINE _h #-}

-- | Order of E-222 curve
_n :: Integer
_n = 0xffffffffffffffffffffffffffff70cbc95e932f802f31423598cbf
{-# INLINE _n #-}

-- | Characteristic of E-222 curve
_p :: Integer
_p = 0x3fffffffffffffffffffffffffffffffffffffffffffffffffffff8b
{-# INLINE _p #-}
