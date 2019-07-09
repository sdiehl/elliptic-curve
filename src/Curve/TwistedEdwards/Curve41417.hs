module Curve.TwistedEdwards.Curve41417
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

-- | Curve41417 curve
data Curve41417

-- | Field of Curve41417 curve
type Fp = PrimeField 0x3fffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffef

-- | Curve41417 curve is a twisted Edwards curve
instance TECurve Curve41417 Fp where
  a_ = const _a
  {-# INLINE a_ #-}
  d_ = const _d
  {-# INLINE d_ #-}
  g_ = _g
  {-# INLINE g_ #-}

-- | Point of Curve41417 curve
type P = TEPoint Curve41417 Fp

-------------------------------------------------------------------------------
-- Parameters
-------------------------------------------------------------------------------

-- | Coefficient @A@ of Curve41417 curve
_a :: Fp
_a = 1
{-# INLINE _a #-}

-- | Coefficient @D@ of Curve41417 curve
_d :: Fp
_d = 0xe21
{-# INLINE _d #-}

-- | Generator of Curve41417 curve
_g :: P
_g = A 0x1a334905141443300218c0631c326e5fcd46369f44c03ec7f57ff35498a4ab4d6d6ba111301a73faa8537c64c4fd3812f3cbc595
       0x22
{-# INLINE _g #-}

-- | Cofactor of Curve41417 curve
_h :: Integer
_h = 8
{-# INLINE _h #-}

-- | Order of Curve41417 curve
_n :: Integer
_n = notImplemented
{-# INLINE _n #-}

-- | Characteristic of Curve41417 curve
_p :: Integer
_p = 0x3fffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffef
{-# INLINE _p #-}
