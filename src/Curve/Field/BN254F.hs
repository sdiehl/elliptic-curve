module Curve.Field.BN254F
  ( Fp12
  , P
  , _g
  , _h
  , _n
  , _p
  ) where

import Protolude

import ExtensionField (ExtensionField, IrreducibleMonic(..), t, x)

import Curve.Field (FCurve(..), FPoint)
import Curve.Weierstrass.BN254T (Fp2)

-------------------------------------------------------------------------------
-- Types
-------------------------------------------------------------------------------

-- | BN254F curve
data BN254F

-- | Field of BN254F curve
data PolynomialV
instance IrreducibleMonic Fp2 PolynomialV where
  split _ = x ^ (3 :: Int) - (9 + t x)
type Fp6 = ExtensionField Fp2 PolynomialV
data PolynomialW
instance IrreducibleMonic Fp6 PolynomialW where
  split _ = x ^ (2 :: Int) - t x
type Fp12 = ExtensionField Fp6 PolynomialW 

-- | BN254F curve is a field curve
instance FCurve BN254F Fp12 where
  g_ = _g
  {-# INLINE g_ #-}
  h_ = const _h
  {-# INLINE h_ #-}
  n_ = const _n
  {-# INLINE n_ #-}
  p_ = const _p
  {-# INLINE p_ #-}

-- | Point of BN254F curve
type P = FPoint BN254F Fp12

-------------------------------------------------------------------------------
-- Parameters
-------------------------------------------------------------------------------

-- | Generator of BN254F curve
_g :: P
_g = panic "not implemented."
{-# INLINE _g #-}

-- | Cofactor of BN254F curve
_h :: Integer
_h = panic "not implemented."
{-# INLINE _h #-}

-- | Order of BN254F curve
_n :: Integer
_n = panic "not implemented."
{-# INLINE _n #-}

-- | Characteristic of BN254F curve
_p :: Integer
_p = 0x30644e72e131a029b85045b68181585d97816a916871ca8d3c208c16d87cfd47
{-# INLINE _p #-}
