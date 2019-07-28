module Curve.Weierstrass.SECP128R1
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

-- | SECP128R1 curve.
data SECP128R1

-- | Field of points of SECP128R1 curve.
type Fq = PrimeField 0xfffffffdffffffffffffffffffffffff

-- | Field of coefficients of SECP128R1 curve.
type Fr = PrimeField 0xfffffffe0000000075a30d1b9038a115

-- | SECP128R1 curve is a Weierstrass curve.
instance WCurve SECP128R1 Fq where
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

-- | Point of SECP128R1 curve.
type P = WPoint SECP128R1 Fq

-------------------------------------------------------------------------------
-- Parameters
-------------------------------------------------------------------------------

-- | Coefficient @A@ of SECP128R1 curve.
_a :: Fq
_a = 0xfffffffdfffffffffffffffffffffffc
{-# INLINE _a #-}

-- | Coefficient @B@ of SECP128R1 curve.
_b :: Fq
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

-- | Characteristic of SECP128R1 curve.
_q :: Integer
_q = 0xfffffffdffffffffffffffffffffffff
{-# INLINE _q #-}

-- | Order of SECP128R1 curve.
_r :: Integer
_r = 0xfffffffe0000000075a30d1b9038a115
{-# INLINE _r #-}

-- | Coordinate @X@ of SECP128R1 curve.
_x :: Fq
_x = 0x161ff7528b899b2d0c28607ca52c5b86
{-# INLINE _x #-}

-- | Coordinate @Y@ of SECP128R1 curve.
_y :: Fq
_y = 0xcf5ac8395bafeb13c02da292dded7a83
{-# INLINE _y #-}
