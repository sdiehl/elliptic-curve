module Curve.Field.BN254TF
  ( Fp12
  , _p
  ) where

import Protolude

import ExtensionField (ExtensionField, IrreducibleMonic(..), t, x)

import Curve.Weierstrass.BN254T (Fp2)

-------------------------------------------------------------------------------
-- Types
-------------------------------------------------------------------------------

-- | BN254TF group
data PolynomialV
instance IrreducibleMonic Fp2 PolynomialV where
  split _ = x ^ (3 :: Int) - (9 + t x)
type Fp6 = ExtensionField Fp2 PolynomialV
data PolynomialW
instance IrreducibleMonic Fp6 PolynomialW where
  split _ = x ^ (2 :: Int) - t x
type Fp12 = ExtensionField Fp6 PolynomialW 

-------------------------------------------------------------------------------
-- Parameters
-------------------------------------------------------------------------------

-- | Characteristic of BN254TF curve
_p :: Integer
_p = 0x30644e72e131a029b85045b68181585d97816a916871ca8d3c208c16d87cfd47
{-# INLINE _p #-}
