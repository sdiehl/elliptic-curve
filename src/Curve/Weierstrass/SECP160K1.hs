module Curve.Weierstrass.SECP160K1
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

-- | SECP160K1 curve
data SECP160K1

-- | Field of SECP160K1 curve
type Fp = PrimeField 0xfffffffffffffffffffffffffffffffeffffac73

-- | SECP160K1 curve is a Weierstrass curve
instance WCurve SECP160K1 Fp where
  a_ = const _a
  {-# INLINE a_ #-}
  b_ = const _b
  {-# INLINE b_ #-}
  g_ = _g
  {-# INLINE g_ #-}

-- | Point of SECP160K1 curve
type P = WPoint SECP160K1 Fp

-------------------------------------------------------------------------------
-- Parameters
-------------------------------------------------------------------------------

-- | Coefficient @A@ of SECP160K1 curve
_a :: Fp
_a = 0
{-# INLINE _a #-}

-- | Coefficient @B@ of SECP160K1 curve
_b :: Fp
_b = 7
{-# INLINE _b #-}

-- | Generator of SECP160K1 curve
_g :: P
_g = A
     0x3b4c382ce37aa192a4019e763036f4f5dd4d7ebb
     0x938cf935318fdced6bc28286531733c3f03c4fee
{-# INLINE _g #-}

-- | Cofactor of SECP160K1 curve
_h :: Integer
_h = 1
{-# INLINE _h #-}

-- | Order of SECP160K1 curve
_n :: Integer
_n = 0x100000000000000000001b8fa16dfab9aca16b6b3
{-# INLINE _n #-}

-- | Characteristic of SECP160K1 curve
_p :: Integer
_p = 0xfffffffffffffffffffffffffffffffeffffac73
{-# INLINE _p #-}
