module Curve.ShortWeierstrass.SECP128R1
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

-- | SECP128R1 curve
data SECP128R1

-- | Field of SECP128R1 curve
type Fp = PrimeField 0xfffffffdffffffffffffffffffffffff

-- | SECP128R1 curve is a short Weierstrass curve
instance SWCurve SECP128R1 Fp where
  a_ = const _a
  {-# INLINE a_ #-}
  b_ = const _b
  {-# INLINE b_ #-}
  g_ = _g
  {-# INLINE g_ #-}

-- | Point of SECP128R1 curve
type P = SWPoint SECP128R1 Fp

-------------------------------------------------------------------------------
-- Parameters
-------------------------------------------------------------------------------

-- | Coefficient @A@ of SECP128R1 curve
_a :: Fp
_a = 0xfffffffdfffffffffffffffffffffffc
{-# INLINE _a #-}

-- | Coefficient @B@ of SECP128R1 curve
_b :: Fp
_b = 0xe87579c11079f43dd824993c2cee5ed3
{-# INLINE _b #-}

-- | Generator of SECP128R1 curve
_g :: P
_g = A
     0x161ff7528b899b2d0c28607ca52c5b86
     0xcf5ac8395bafeb13c02da292dded7a83
{-# INLINE _g #-}

-- | Cofactor of SECP128R1 curve
_h :: Integer
_h = 1
{-# INLINE _h #-}

-- | Order of SECP128R1 curve
_n :: Integer
_n = 0xfffffffe0000000075a30d1b9038a115
{-# INLINE _n #-}

-- | Characteristic of SECP128R1 curve
_p :: Integer
_p = 0xfffffffdffffffffffffffffffffffff
{-# INLINE _p #-}
