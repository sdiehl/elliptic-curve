module Curve.ShortWeierstrass.SECP192R1
  -- | Imports
  ( Point(..)
  , SWCurve(..)
  , SWPoint
  -- | Types
  , Fp
  , P
  ) where

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
  _a _ = 0xfffffffffffffffffffffffffffffffefffffffffffffffc
  {-# INLINE _a #-}
  _b _ = 0x64210519e59c80e70fa7e9ab72243049feb8deecc146b9b1
  {-# INLINE _b #-}
  _g   = A 0x188da80eb03090f67cbf20eb43a18800f4ff0afd82ff1012
           0x07192b95ffc8da78631011ed6b24cdd573f977a11e794811
  {-# INLINE _g #-}
  _h _ = 1
  {-# INLINE _h #-}
  _n _ = 0xffffffffffffffffffffffff99def836146bc9b1b4d22831
  {-# INLINE _n #-}
  _p _ = 0xfffffffffffffffffffffffffffffffeffffffffffffffff
  {-# INLINE _p #-}

-- | Point of SECP192R1 curve
type P = SWPoint SECP192R1 Fp
