module Curve.Edwards.Curve41417
  ( Fp
  , P
  , _a
  , _d
  , _g
  , _h
  , _n
  , _p
  ) where

import Protolude

import PrimeField (PrimeField)

import Curve.Edwards (ECurve(..), EPoint, Point(..))

-------------------------------------------------------------------------------
-- Types
-------------------------------------------------------------------------------

-- | Curve41417 curve
data Curve41417

-- | Field of Curve41417 curve
type Fp = PrimeField 0x3fffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffef

-- | Curve41417 curve is an Edwards curve
instance ECurve Curve41417 Fp where
  a_ = const _a
  {-# INLINE a_ #-}
  d_ = const _d
  {-# INLINE d_ #-}
  g_ = _g
  {-# INLINE g_ #-}
  h_ = const _h
  {-# INLINE h_ #-}
  n_ = const _n
  {-# INLINE n_ #-}
  p_ = const _p
  {-# INLINE p_ #-}

-- | Point of Curve41417 curve
type P = EPoint Curve41417 Fp

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
_g = A
     0x1a334905141443300218c0631c326e5fcd46369f44c03ec7f57ff35498a4ab4d6d6ba111301a73faa8537c64c4fd3812f3cbc595
     0x22
{-# INLINE _g #-}

-- | Cofactor of Curve41417 curve
_h :: Integer
_h = 8
{-# INLINE _h #-}

-- | Order of Curve41417 curve
_n :: Integer
_n = 0x7ffffffffffffffffffffffffffffffffffffffffffffffffffeb3cc92414cf706022b36f1c0338ad63cf181b0e71a5e106af79
{-# INLINE _n #-}

-- | Characteristic of Curve41417 curve
_p :: Integer
_p = 0x3fffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffef
{-# INLINE _p #-}
