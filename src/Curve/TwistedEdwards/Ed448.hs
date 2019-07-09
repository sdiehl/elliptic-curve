module Curve.TwistedEdwards.Ed448
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

-- | Ed448 curve
data Ed448

-- | Field of Ed448 curve
type Fp = PrimeField 0xfffffffffffffffffffffffffffffffffffffffffffffffffffffffeffffffffffffffffffffffffffffffffffffffffffffffffffffffff

-- | Ed448 curve is a twisted Edwards curve
instance TECurve Ed448 Fp where
  a_ = const _a
  {-# INLINE a_ #-}
  d_ = const _d
  {-# INLINE d_ #-}
  g_ = _g
  {-# INLINE g_ #-}

-- | Point of Ed448 curve
type P = TEPoint Ed448 Fp

-------------------------------------------------------------------------------
-- Parameters
-------------------------------------------------------------------------------

-- | Coefficient @A@ of Ed448 curve
_a :: Fp
_a = 1
{-# INLINE _a #-}

-- | Coefficient @D@ of Ed448 curve
_d :: Fp
_d = 0xfffffffffffffffffffffffffffffffffffffffffffffffffffffffeffffffffffffffffffffffffffffffffffffffffffffffffffff6756
{-# INLINE _d #-}

-- | Generator of Ed448 curve
_g :: P
_g = A 0x297ea0ea2692ff1b4faff46098453a6a26adf733245f065c3c59d0709cecfa96147eaaf3932d94c63d96c170033f4ba0c7f0de840aed939f
       0x13
{-# INLINE _g #-}

-- | Cofactor of Ed448 curve
_h :: Integer
_h = 4
{-# INLINE _h #-}

-- | Order of Ed448 curve
_n :: Integer
_n = 0x3fffffffffffffffffffffffffffffffffffffffffffffffffffffff7cca23e9c44edb49aed63690216cc2728dc58f552378c292ab5844f3
{-# INLINE _n #-}

-- | Characteristic of Ed448 curve
_p :: Integer
_p = 0xfffffffffffffffffffffffffffffffffffffffffffffffffffffffeffffffffffffffffffffffffffffffffffffffffffffffffffffffff
{-# INLINE _p #-}
