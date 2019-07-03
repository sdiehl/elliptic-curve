module Curve.ShortWeierstrass.BLS12_381.G1
  -- | Imports
  ( Curve(..)
  , P(..)
  , SWCurve(..)
  -- | Types
  , Fq
  , Point
  -- | Parameters
  , _b
  , _h
  ) where

import Protolude

import PrimeField (PrimeField)

import Curve (Curve(..))
import Curve.ShortWeierstrass (P(..), SWCurve(..))

-------------------------------------------------------------------------------
-- Types
-------------------------------------------------------------------------------

-- | BLS12-381 curve G1 @y^2=x^3+4@
data G1

-- | Prime field @Fq@ of BLS12-381 curve G1
type Fq = PrimeField 0x1a0111ea397fe69a4b1ba7b6434bacd764774b84f38512bf6730d2a0f6b0f6241eabfffeb153ffffb9feffffffffaaab

-- | BLS12-381 curve G1 is a short Weierstrass curve
instance SWCurve G1 Fq where
  a _ = 0
  b _ = _b

-- | Point of BLS12-381 curve G1
type Point = P G1 Fq

-------------------------------------------------------------------------------
-- Parameters
-------------------------------------------------------------------------------

-- | Coefficient of BLS12-381 curve G1
_b :: Fq
_b = 4
{-# INLINE _b #-}

-- | Cofactor of BLS12-381 curve G1
_h :: Integer
_h = 0x396c8c005555e1568c00aaab0000aaa
{-# INLINE _h #-}
