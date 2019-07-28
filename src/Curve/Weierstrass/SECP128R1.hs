module Curve.Weierstrass.SECP128R1
  ( Curve(..)
  , Fp
  , Group(..)
  , P
  , Point(..)
  , WPoint
  , WCurve(..)
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

import Curve (Curve(..))
import Curve.Weierstrass (Point(..), WCurve(..), WPoint)
import Group (Group(..))

-------------------------------------------------------------------------------
-- Types
-------------------------------------------------------------------------------

-- | SECP128R1 curve.
data SECP128R1

-- | Field of SECP128R1 curve.
type Fp = PrimeField 0xfffffffdffffffffffffffffffffffff

-- | SECP128R1 curve is a Weierstrass curve.
instance WCurve SECP128R1 Fp where
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
  x_ = const _x
  {-# INLINE x_ #-}
  y_ = const _y
  {-# INLINE y_ #-}

-- | Point of SECP128R1 curve.
type P = WPoint SECP128R1 Fp

-------------------------------------------------------------------------------
-- Parameters
-------------------------------------------------------------------------------

-- | Coefficient @A@ of SECP128R1 curve.
_a :: Fp
_a = 0xfffffffdfffffffffffffffffffffffc
{-# INLINE _a #-}

-- | Coefficient @B@ of SECP128R1 curve.
_b :: Fp
_b = 0xe87579c11079f43dd824993c2cee5ed3
{-# INLINE _b #-}

-- | Generator of SECP128R1 curve.
_g :: P
_g = A _x _y
{-# INLINE _g #-}

-- | Cofactor of SECP128R1 curve.
_h :: Integer
_h = 0x1
{-# INLINE _h #-}

-- | Order of SECP128R1 curve.
_n :: Integer
_n = 0xfffffffe0000000075a30d1b9038a115
{-# INLINE _n #-}

-- | Characteristic of SECP128R1 curve.
_p :: Integer
_p = 0xfffffffdffffffffffffffffffffffff
{-# INLINE _p #-}

-- | Coordinate @X@ of SECP128R1 curve.
_x :: Fp
_x = 0x161ff7528b899b2d0c28607ca52c5b86
{-# INLINE _x #-}

-- | Coordinate @Y@ of SECP128R1 curve.
_y :: Fp
_y = 0xcf5ac8395bafeb13c02da292dded7a83
{-# INLINE _y #-}
