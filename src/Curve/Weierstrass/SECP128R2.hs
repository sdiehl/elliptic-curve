module Curve.Weierstrass.SECP128R2
  ( Fp
  , P
  , _a
  , _b
  , _g
  , _h
  , _n
  , _p
  , _x
  , _y
  ) where

import Protolude

import PrimeField (PrimeField)

import Curve.Weierstrass (Point(..), WCurve(..), WPoint)

-------------------------------------------------------------------------------
-- Types
-------------------------------------------------------------------------------

-- | SECP128R2 curve.
data SECP128R2

-- | Field of SECP128R2 curve.
type Fp = PrimeField 0xfffffffdffffffffffffffffffffffff

-- | SECP128R2 curve is a Weierstrass curve.
instance WCurve SECP128R2 Fp where
  a_ = const _a
  {-# INLINE a_ #-}
  b_ = const _b
  {-# INLINE b_ #-}
  g_ = _g
  {-# INLINE g_ #-}
  h_ = const _h
  {-# INLINE h_ #-}
  n_ = const _n
  {-# INLINE n_ #-}
  p_ = const _p
  {-# INLINE p_ #-}

-- | Point of SECP128R2 curve.
type P = WPoint SECP128R2 Fp

-------------------------------------------------------------------------------
-- Parameters
-------------------------------------------------------------------------------

-- | Coefficient @A@ of SECP128R2 curve.
_a :: Fp
_a = 0xd6031998d1b3bbfebf59cc9bbff9aee1
{-# INLINE _a #-}

-- | Coefficient @B@ of SECP128R2 curve.
_b :: Fp
_b = 0x5eeefca380d02919dc2c6558bb6d8a5d
{-# INLINE _b #-}

-- | Generator of SECP128R2 curve.
_g :: P
_g = A _x _y
{-# INLINE _g #-}

-- | Cofactor of SECP128R2 curve.
_h :: Integer
_h = 0x4
{-# INLINE _h #-}

-- | Order of SECP128R2 curve.
_n :: Integer
_n = 0x3fffffff7fffffffbe0024720613b5a3
{-# INLINE _n #-}

-- | Characteristic of SECP128R2 curve.
_p :: Integer
_p = 0xfffffffdffffffffffffffffffffffff
{-# INLINE _p #-}

-- | Coordinate @X@ of SECP128R2 curve.
_x :: Fp
_x = 0x7b6aa5d85e572983e6fb32a7cdebc140
{-# INLINE _x #-}

-- | Coordinate @Y@ of SECP128R2 curve.
_y :: Fp
_y = 0x27b6916a894d3aee7106fe805fc34b44
{-# INLINE _y #-}
