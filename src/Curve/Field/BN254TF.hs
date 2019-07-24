module Curve.Field.BN254TF
  ( Fp12
  , P
  , _g
  , _n
  , _p
  ) where

import Protolude

import ExtensionField (ExtensionField, IrreducibleMonic(..), t, x)

import Curve.Field (FGroup(..), Element(..))
import Curve.Weierstrass.BN254T (Fp2)

-------------------------------------------------------------------------------
-- Types
-------------------------------------------------------------------------------

-- | Field of BN254TF group
data PolynomialV
instance IrreducibleMonic Fp2 PolynomialV where
  split _ = x ^ (3 :: Int) - (9 + t x)
type Fp6 = ExtensionField Fp2 PolynomialV
data PolynomialW
instance IrreducibleMonic Fp6 PolynomialW where
  split _ = x ^ (2 :: Int) - t x
type Fp12 = ExtensionField Fp6 PolynomialW

-- | BN254TF group is a field group
instance FGroup Fp12 where
  g_ = _g
  {-# INLINE g_ #-}
  n_ = const _n
  {-# INLINE n_ #-}
  p_ = const _p
  {-# INLINE p_ #-}

-- | Element of BN254TF group
type P = Element Fp12

-------------------------------------------------------------------------------
-- Parameters
-------------------------------------------------------------------------------

-- | Generator of BN254TF group
_g :: P
_g = F 1 -- TODO
{-# INLINE _g #-}

-- | Order of BN254TF group
_n :: Integer
_n = 0x30644e72e131a029b85045b68181585d2833e84879b9709143e1f593f0000001
{-# INLINE _n #-}

-- | Characteristic of BN254TF group
_p :: Integer
_p = 0x30644e72e131a029b85045b68181585d97816a916871ca8d3c208c16d87cfd47
{-# INLINE _p #-}
