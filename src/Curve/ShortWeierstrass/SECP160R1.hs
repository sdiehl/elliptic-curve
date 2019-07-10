module Curve.ShortWeierstrass.SECP160R1
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

-- | SECP160R1 curve
data SECP160R1

-- | Field of SECP160R1 curve
type Fp = PrimeField 0xffffffffffffffffffffffffffffffff7fffffff

-- | SECP160R1 curve is a short Weierstrass curve
instance SWCurve SECP160R1 Fp where
  a_ = const _a
  {-# INLINE a_ #-}
  b_ = const _b
  {-# INLINE b_ #-}
  g_ = _g
  {-# INLINE g_ #-}

-- | Point of SECP160R1 curve
type P = SWPoint SECP160R1 Fp

-------------------------------------------------------------------------------
-- Parameters
-------------------------------------------------------------------------------

-- | Coefficient @A@ of SECP160R1 curve
_a :: Fp
_a = 0xffffffffffffffffffffffffffffffff7ffffffc
{-# INLINE _a #-}

-- | Coefficient @B@ of SECP160R1 curve
_b :: Fp
_b = 0x1c97befc54bd7a8b65acf89f81d4d4adc565fa45
{-# INLINE _b #-}

-- | Generator of SECP160R1 curve
_g :: P
_g = A
     0x4a96b5688ef573284664698968c38bb913cbfc82
     0x23a628553168947d59dcc912042351377ac5fb32
{-# INLINE _g #-}

-- | Cofactor of SECP160R1 curve
_h :: Integer
_h = 1
{-# INLINE _h #-}

-- | Order of SECP160R1 curve
_n :: Integer
_n = 0x100000000000000000001f4c8f927aed3ca752257
{-# INLINE _n #-}

-- | Characteristic of SECP160R1 curve
_p :: Integer
_p = 0xffffffffffffffffffffffffffffffff7fffffff
{-# INLINE _p #-}
