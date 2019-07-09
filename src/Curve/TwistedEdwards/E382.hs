module Curve.TwistedEdwards.E382
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

-- | E-382 curve
data E382

-- | Field of E-382 curve
type Fp = PrimeField 0x3fffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff97

-- | E-382 curve is a twisted Edwards curve
instance TECurve E382 Fp where
  a_ = const _a
  {-# INLINE a_ #-}
  d_ = const _d
  {-# INLINE d_ #-}
  g_ = _g
  {-# INLINE g_ #-}

-- | Point of E-382 curve
type P = TEPoint E382 Fp

-------------------------------------------------------------------------------
-- Parameters
-------------------------------------------------------------------------------

-- | Coefficient @A@ of E-382 curve
_a :: Fp
_a = 1
{-# INLINE _a #-}

-- | Coefficient @D@ of E-382 curve
_d :: Fp
_d = 0x3ffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffef8e1
{-# INLINE _d #-}

-- | Generator of E-382 curve
_g :: P
_g = A 0x196f8dd0eab20391e5f05be96e8d20ae68f840032b0b64352923bab85364841193517dbce8105398ebc0cc9470f79603
       0x11
{-# INLINE _g #-}

-- | Cofactor of E-382 curve
_h :: Integer
_h = 4
{-# INLINE _h #-}

-- | Order of E-382 curve
_n :: Integer
_n = 0xfffffffffffffffffffffffffffffffffffffffffffffffd5fb21f21e95eee17c5e69281b102d2773e27e13fd3c9719
{-# INLINE _n #-}

-- | Characteristic of E-382 curve
_p :: Integer
_p = 0x3fffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff97
{-# INLINE _p #-}
