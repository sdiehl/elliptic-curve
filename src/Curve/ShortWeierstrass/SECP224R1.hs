module Curve.ShortWeierstrass.SECP224R1
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

-- | SECP224R1 curve
data SECP224R1

-- | Field of SECP224R1 curve
type Fp = PrimeField 0xffffffffffffffffffffffffffffffff000000000000000000000001

-- | SECP224R1 curve is a short Weierstrass curve
instance SWCurve SECP224R1 Fp where
  a_ = const _a
  {-# INLINE a_ #-}
  b_ = const _b
  {-# INLINE b_ #-}
  g_ = _g
  {-# INLINE g_ #-}

-- | Point of SECP224R1 curve
type P = SWPoint SECP224R1 Fp

-------------------------------------------------------------------------------
-- Parameters
-------------------------------------------------------------------------------

-- | Coefficient @A@ of SECP224R1 curve
_a :: Fp
_a = 0xfffffffffffffffffffffffffffffffefffffffffffffffffffffffe
{-# INLINE _a #-}

-- | Coefficient @B@ of SECP224R1 curve
_b :: Fp
_b = 0xb4050a850c04b3abf54132565044b0b7d7bfd8ba270b39432355ffb4
{-# INLINE _b #-}

-- | Generator of SECP224R1 curve
_g :: P
_g = A
     0xb70e0cbd6bb4bf7f321390b94a03c1d356c21122343280d6115c1d21
     0xbd376388b5f723fb4c22dfe6cd4375a05a07476444d5819985007e34
{-# INLINE _g #-}

-- | Cofactor of SECP224R1 curve
_h :: Integer
_h = 1
{-# INLINE _h #-}

-- | Order of SECP224R1 curve
_n :: Integer
_n = 0xffffffffffffffffffffffffffff16a2e0b8f03e13dd29455c5c2a3d
{-# INLINE _n #-}

-- | Characteristic of SECP224R1 curve
_p :: Integer
_p = 0xffffffffffffffffffffffffffffffff000000000000000000000001
{-# INLINE _p #-}
