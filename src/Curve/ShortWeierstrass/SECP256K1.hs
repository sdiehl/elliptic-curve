module Curve.ShortWeierstrass.SECP256K1
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

import Curve.ShortWeierstrass (Point(..), SWCurve(..), SWPoint)

-------------------------------------------------------------------------------
-- Types
-------------------------------------------------------------------------------

-- | SECP256K1 curve
data SECP256K1

-- | Field of SECP256K1 curve
type Fp = PrimeField 0xfffffffffffffffffffffffffffffffffffffffffffffffffffffffefffffc2f

-- | SECP256K1 curve is a short Weierstrass curve
instance SWCurve SECP256K1 Fp where
  a_ = const _a
  {-# INLINE a_ #-}
  b_ = const _b
  {-# INLINE b_ #-}
  g_ = _g
  {-# INLINE g_ #-}

-- | Point of SECP256K1 curve
type P = SWPoint SECP256K1 Fp

-------------------------------------------------------------------------------
-- Parameters
-------------------------------------------------------------------------------

-- | Coefficient @A@ of SECP256K1 curve
_a :: Fp
_a = 0
{-# INLINE _a #-}

-- | Coefficient @B@ of SECP256K1 curve
_b :: Fp
_b = 7
{-# INLINE _b #-}

-- | Generator of SECP256K1 curve
_g :: P
_g = A
     0x79be667ef9dcbbac55a06295ce870b07029bfcdb2dce28d959f2815b16f81798
     0x483ada7726a3c4655da4fbfc0e1108a8fd17b448a68554199c47d08ffb10d4b8
{-# INLINE _g #-}

-- | Cofactor of SECP256K1 curve
_h :: Integer
_h = 1
{-# INLINE _h #-}

-- | Order of SECP256K1 curve
_n :: Integer
_n = 0xfffffffffffffffffffffffffffffffebaaedce6af48a03bbfd25e8cd0364141
{-# INLINE _n #-}

-- | Characteristic of SECP256K1 curve
_p :: Integer
_p = 0xfffffffffffffffffffffffffffffffffffffffffffffffffffffffefffffc2f
{-# INLINE _p #-}
