module Curve.Weierstrass.BN224
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

-- | BN224 curve.
data BN224

-- | Field of BN224 curve.
type Fp = PrimeField 0xfffffffffff107288ec29e602c4520db42180823bb907d1287127833

-- | BN224 curve is a Weierstrass curve.
instance WCurve BN224 Fp where
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

-- | Point of BN224 curve.
type P = WPoint BN224 Fp

-------------------------------------------------------------------------------
-- Parameters
-------------------------------------------------------------------------------

-- | Coefficient @A@ of BN224 curve.
_a :: Fp
_a = 0x0
{-# INLINE _a #-}

-- | Coefficient @B@ of BN224 curve.
_b :: Fp
_b = 0x3
{-# INLINE _b #-}

-- | Generator of BN224 curve.
_g :: P
_g = A _x _y
{-# INLINE _g #-}

-- | Cofactor of BN224 curve.
_h :: Integer
_h = 0x1
{-# INLINE _h #-}

-- | Order of BN224 curve.
_n :: Integer
_n = 0xfffffffffff107288ec29e602c4420db4218082b36c2accff76c58ed
{-# INLINE _n #-}

-- | Characteristic of BN224 curve.
_p :: Integer
_p = 0xfffffffffff107288ec29e602c4520db42180823bb907d1287127833
{-# INLINE _p #-}

-- | Coordinate @X@ of BN224 curve.
_x :: Fp
_x = 0x1
{-# INLINE _x #-}

-- | Coordinate @Y@ of BN224 curve.
_y :: Fp
_y = 0x2
{-# INLINE _y #-}
