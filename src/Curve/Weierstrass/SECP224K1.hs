module Curve.Weierstrass.SECP224K1
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

import Curve.Weierstrass (Point(..), WCurve(..), WPoint)

-------------------------------------------------------------------------------
-- Types
-------------------------------------------------------------------------------

-- | SECP224K1 curve
data SECP224K1

-- | Field of SECP224K1 curve
type Fp = PrimeField 0xfffffffffffffffffffffffffffffffffffffffffffffffeffffe56d

-- | SECP224K1 curve is a Weierstrass curve
instance WCurve SECP224K1 Fp where
  a_ = const _a
  {-# INLINE a_ #-}
  b_ = const _b
  {-# INLINE b_ #-}
  g_ = _g
  {-# INLINE g_ #-}

-- | Point of SECP224K1 curve
type P = WPoint SECP224K1 Fp

-------------------------------------------------------------------------------
-- Parameters
-------------------------------------------------------------------------------

-- | Coefficient @A@ of SECP224K1 curve
_a :: Fp
_a = 0
{-# INLINE _a #-}

-- | Coefficient @B@ of SECP224K1 curve
_b :: Fp
_b = 5
{-# INLINE _b #-}

-- | Generator of SECP224K1 curve
_g :: P
_g = A
     0xa1455b334df099df30fc28a169a467e9e47075a90f7e650eb6b7a45c
     0x7e089fed7fba344282cafbd6f7e319f7c0b0bd59e2ca4bdb556d61a5
{-# INLINE _g #-}

-- | Cofactor of SECP224K1 curve
_h :: Integer
_h = 1
{-# INLINE _h #-}

-- | Order of SECP224K1 curve
_n :: Integer
_n = 0x10000000000000000000000000001dce8d2ec6184caf0a971769fb1f7
{-# INLINE _n #-}

-- | Characteristic of SECP224K1 curve
_p :: Integer
_p = 0xfffffffffffffffffffffffffffffffffffffffffffffffeffffe56d
{-# INLINE _p #-}
