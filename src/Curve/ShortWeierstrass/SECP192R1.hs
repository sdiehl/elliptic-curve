module Curve.ShortWeierstrass.SECP192R1
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

-- | SECP192R1 curve
data SECP192R1

-- | Field of SECP192R1 curve
type Fp = PrimeField 0xfffffffffffffffffffffffffffffffeffffffffffffffff

-- | SECP192R1 curve is a short Weierstrass curve
instance SWCurve SECP192R1 Fp where
  a_ = const _a
  {-# INLINE a_ #-}
  b_ = const _b
  {-# INLINE b_ #-}
  g_ = _g
  {-# INLINE g_ #-}

-- | Point of SECP192R1 curve
type P = SWPoint SECP192R1 Fp

-------------------------------------------------------------------------------
-- Parameters
-------------------------------------------------------------------------------

-- | Coefficient @A@ of SECP192R1 curve
_a :: Fp
_a = 0xfffffffffffffffffffffffffffffffefffffffffffffffc
{-# INLINE _a #-}

-- | Coefficient @B@ of SECP192R1 curve
_b :: Fp
_b = 0x64210519e59c80e70fa7e9ab72243049feb8deecc146b9b1
{-# INLINE _b #-}

-- | Generator of SECP192R1 curve
_g :: P
_g = A
     0x188da80eb03090f67cbf20eb43a18800f4ff0afd82ff1012
     0x7192b95ffc8da78631011ed6b24cdd573f977a11e794811
{-# INLINE _g #-}

-- | Cofactor of SECP192R1 curve
_h :: Integer
_h = 1
{-# INLINE _h #-}

-- | Order of SECP192R1 curve
_n :: Integer
_n = 0xffffffffffffffffffffffff99def836146bc9b1b4d22831
{-# INLINE _n #-}

-- | Characteristic of SECP192R1 curve
_p :: Integer
_p = 0xfffffffffffffffffffffffffffffffeffffffffffffffff
{-# INLINE _p #-}
