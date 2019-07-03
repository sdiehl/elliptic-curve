module Curve.ShortWeierstrass.BLS12_381.G2
  -- | Imports
  ( Curve(..)
  , P(..)
  , SWCurve(..)
  -- | Types
  , Fq2
  , Point
  -- | Parameters
  , _b
  , _h
  ) where

import Protolude

import ExtensionField (ExtensionField, IrreducibleMonic(..), fromList, x)

import Curve (Curve(..))
import Curve.ShortWeierstrass (P(..), SWCurve(..))
import Curve.ShortWeierstrass.BLS12_381.G1 (Fq)

-------------------------------------------------------------------------------
-- Types
-------------------------------------------------------------------------------

-- | BLS12-381 curve G2 @y^2=x^3+4(1+i)@
data G2

-- | Extension field @Fq2 = Fq[u] / <u^2 + 1>@ of BLS12-381 curve G2
data PolynomialU
instance IrreducibleMonic Fq PolynomialU where
  split _ = x ^ (2 :: Int) + 1
type Fq2 = ExtensionField Fq PolynomialU

-- | BLS12-381 curve G2 is a short Weierstrass curve
instance SWCurve G2 Fq2 where
  a _ = 0
  b _ = _b

-- | Point of BLS12-381 curve G2
type Point = P G2 Fq2

-------------------------------------------------------------------------------
-- Parameters
-------------------------------------------------------------------------------

-- | Coefficient of BLS12-381 curve G2
_b :: Fq2
_b = fromList [4, 1]
{-# INLINE _b #-}

-- | Cofactor of BLS12-381 curve G2
_h :: Integer
_h = 0x5d543a95414e7f1091d50792876a202cd91de4547085abaa68a205b2e5a7ddfa628f1cb4d9e82ef21537e293a6691ae1616ec6e786f0c70cf1c38e31c7238e
{-# INLINE _h #-}
