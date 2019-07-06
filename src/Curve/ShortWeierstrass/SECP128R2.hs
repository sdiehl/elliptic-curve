module Curve.ShortWeierstrass.SECP128R2
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

-- | SECP128R2 curve
data SECP128R2

-- | Field of SECP128R2 curve
type Fp = PrimeField 0xfffffffdffffffffffffffffffffffff

-- | SECP128R2 curve is a short Weierstrass curve
instance SWCurve SECP128R2 Fp where
  a_ = const _a
  {-# INLINE a_ #-}
  b_ = const _b
  {-# INLINE b_ #-}
  g_ = _g
  {-# INLINE g_ #-}

-- | Point of SECP128R2 curve
type P = SWPoint SECP128R2 Fp

-------------------------------------------------------------------------------
-- Parameters
-------------------------------------------------------------------------------

-- | Coefficient @A@ of SECP128R2 curve
_a :: Fp
_a = 0xd6031998d1b3bbfebf59cc9bbff9aee1
{-# INLINE _a #-}

-- | Coefficient @B@ of SECP128R2 curve
_b :: Fp
_b = 0x5eeefca380d02919dc2c6558bb6d8a5d
{-# INLINE _b #-}

-- | Generator of SECP128R2 curve
_g :: P
_g = A 0x7b6aa5d85e572983e6fb32a7cdebc140
       0x27b6916a894d3aee7106fe805fc34b44
{-# INLINE _g #-}

-- | Cofactor of SECP128R2 curve
_h :: Integer
_h = 4
{-# INLINE _h #-}

-- | Order of SECP128R2 curve
_n :: Integer
_n = 0x3fffffff7fffffffbe0024720613b5a3
{-# INLINE _n #-}

-- | Characteristic of SECP128R2 curve
_p :: Integer
_p = 0xfffffffdffffffffffffffffffffffff
{-# INLINE _p #-}
