module Curve.Montgomery.M511
  -- | Types
  ( Fp
  , P
  -- | Parameters
  , _a
  , _b
  , _g
  , _h
  , _n
  , _p
  ) where

import Protolude

import PrimeField (PrimeField)

import Curve.Montgomery (Point(..), MCurve(..), MPoint)

-------------------------------------------------------------------------------
-- Types
-------------------------------------------------------------------------------

-- | M-511 curve
data M511

-- | Field of M-511 curve
type Fp = PrimeField 0x7fffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff45

-- | M-511 curve is a Montgomery curve
instance MCurve M511 Fp where
  a_ = const _a
  {-# INLINE a_ #-}
  b_ = const _b
  {-# INLINE b_ #-}
  g_ = _g
  {-# INLINE g_ #-}

-- | Point of M-511 curve
type P = MPoint M511 Fp

-------------------------------------------------------------------------------
-- Parameters
-------------------------------------------------------------------------------

-- | Coefficient @A@ of M-511 curve
_a :: Fp
_a = 0x81806
{-# INLINE _a #-}

-- | Coefficient @B@ of M-511 curve
_b :: Fp
_b = 1
{-# INLINE _b #-}

-- | Generator of M-511 curve
_g :: P
_g = A
     5
     0x2fbdc0ad8530803d28fdbad354bb488d32399ac1cf8f6e01ee3f96389b90c809422b9429e8a43dbf49308ac4455940abe9f1dbca542093a895e30a64af056fa5
{-# INLINE _g #-}

-- | Cofactor of M-511 curve
_h :: Integer
_h = 8
{-# INLINE _h #-}

-- | Order of M-511 curve
_n :: Integer
_n = 0x100000000000000000000000000000000000000000000000000000000000000017b5feff30c7f5677ab2aeebd13779a2ac125042a6aa10bfa54c15bab76baf1b
{-# INLINE _n #-}

-- | Characteristic of M-511 curve
_p :: Integer
_p = 0x7fffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff45
{-# INLINE _p #-}
