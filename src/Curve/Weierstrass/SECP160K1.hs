module Curve.Weierstrass.SECP160K1
  ( Curve(..)
  , Fq
  , Fr
  , Group(..)
  , P
  , Point(..)
  , WPoint
  , WCurve(..)
  , _a
  , _b
  , _g
  , _h
  , _q
  , _r
  , _x
  , _y
  ) where

import Protolude

import PrimeField (PrimeField)

import Curve (Curve(..))
import Curve.Weierstrass (Point(..), WCurve(..), WPoint)
import Group (Group(..))

-------------------------------------------------------------------------------
-- Types
-------------------------------------------------------------------------------

-- | SECP160K1 curve.
data SECP160K1

-- | Field of points of SECP160K1 curve.
type Fq = PrimeField 0xfffffffffffffffffffffffffffffffeffffac73

-- | Field of coefficients of SECP160K1 curve.
type Fr = PrimeField 0x100000000000000000001b8fa16dfab9aca16b6b3

-- | SECP160K1 curve is a Weierstrass curve.
instance WCurve SECP160K1 Fq where
  a_ = const _a
  {-# INLINE a_ #-}
  b_ = const _b
  {-# INLINE b_ #-}
  g_ = _g
  {-# INLINE g_ #-}
  h_ = const _h
  {-# INLINE h_ #-}
  q_ = const _q
  {-# INLINE q_ #-}
  r_ = const _r
  {-# INLINE r_ #-}
  x_ = const _x
  {-# INLINE x_ #-}
  y_ = const _y
  {-# INLINE y_ #-}

-- | Point of SECP160K1 curve.
type P = WPoint SECP160K1 Fq

-------------------------------------------------------------------------------
-- Parameters
-------------------------------------------------------------------------------

-- | Coefficient @A@ of SECP160K1 curve.
_a :: Fq
_a = 0x0
{-# INLINE _a #-}

-- | Coefficient @B@ of SECP160K1 curve.
_b :: Fq
_b = 0x7
{-# INLINE _b #-}

-- | Generator of SECP160K1 curve.
_g :: P
_g = A _x _y
{-# INLINE _g #-}

-- | Cofactor of SECP160K1 curve.
_h :: Integer
_h = 0x1
{-# INLINE _h #-}

-- | Characteristic of SECP160K1 curve.
_q :: Integer
_q = 0xfffffffffffffffffffffffffffffffeffffac73
{-# INLINE _q #-}

-- | Order of SECP160K1 curve.
_r :: Integer
_r = 0x100000000000000000001b8fa16dfab9aca16b6b3
{-# INLINE _r #-}

-- | Coordinate @X@ of SECP160K1 curve.
_x :: Fq
_x = 0x3b4c382ce37aa192a4019e763036f4f5dd4d7ebb
{-# INLINE _x #-}

-- | Coordinate @Y@ of SECP160K1 curve.
_y :: Fq
_y = 0x938cf935318fdced6bc28286531733c3f03c4fee
{-# INLINE _y #-}
