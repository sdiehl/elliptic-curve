module Curve.Edwards.E382
  ( Curve(..)
  , EPoint
  , ECurve(..)
  , Fp
  , Group(..)
  , P
  , Point(..)
  , _a
  , _d
  , _g
  , _h
  , _n
  , _p
  , _x
  , _y
  ) where

import Protolude

import PrimeField (PrimeField)

import Curve (Curve(..), Group(..))
import Curve.Edwards (ECurve(..), EPoint, Point(..))

-------------------------------------------------------------------------------
-- Types
-------------------------------------------------------------------------------

-- | E382 curve.
data E382

-- | Field of E382 curve.
type Fp = PrimeField 0x3fffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff97

-- | E382 curve is an Edwards curve.
instance ECurve E382 Fp where
  a_ = const _a
  {-# INLINE a_ #-}
  d_ = const _d
  {-# INLINE d_ #-}
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

-- | Point of E382 curve.
type P = EPoint E382 Fp

-------------------------------------------------------------------------------
-- Parameters
-------------------------------------------------------------------------------

-- | Coefficient @A@ of E382 curve.
_a :: Fp
_a = 0x1
{-# INLINE _a #-}

-- | Coefficient @B@ of E382 curve.
_d :: Fp
_d = 0x3ffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffef8e1
{-# INLINE _d #-}

-- | Generator of E382 curve.
_g :: P
_g = A _x _y
{-# INLINE _g #-}

-- | Cofactor of E382 curve.
_h :: Integer
_h = 0x4
{-# INLINE _h #-}

-- | Order of E382 curve.
_n :: Integer
_n = 0xfffffffffffffffffffffffffffffffffffffffffffffffd5fb21f21e95eee17c5e69281b102d2773e27e13fd3c9719
{-# INLINE _n #-}

-- | Characteristic of E382 curve.
_p :: Integer
_p = 0x3fffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff97
{-# INLINE _p #-}

-- | Coordinate @X@ of E382 curve.
_x :: Fp
_x = 0x196f8dd0eab20391e5f05be96e8d20ae68f840032b0b64352923bab85364841193517dbce8105398ebc0cc9470f79603
{-# INLINE _x #-}

-- | Coordinate @Y@ of E382 curve.
_y :: Fp
_y = 0x11
{-# INLINE _y #-}
