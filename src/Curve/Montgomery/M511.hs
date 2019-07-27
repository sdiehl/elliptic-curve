module Curve.Montgomery.M511
  ( Curve(..)
  , Fp
  , Group(..)
  , MPoint
  , MCurve(..)
  , P
  , Point(..)
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

import Curve (Curve(..), Group(..))
import Curve.Montgomery (MCurve(..), MPoint, Point(..))

-------------------------------------------------------------------------------
-- Types
-------------------------------------------------------------------------------

-- | M511 curve.
data M511

-- | Field of M511 curve.
type Fp = PrimeField 0x7fffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff45

-- | M511 curve is a Montgomery curve.
instance MCurve M511 Fp where
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

-- | Point of M511 curve.
type P = MPoint M511 Fp

-------------------------------------------------------------------------------
-- Parameters
-------------------------------------------------------------------------------

-- | Coefficient @A@ of M511 curve.
_a :: Fp
_a = 0x81806
{-# INLINE _a #-}

-- | Coefficient @B@ of M511 curve.
_b :: Fp
_b = 0x1
{-# INLINE _b #-}

-- | Generator of M511 curve.
_g :: P
_g = A _x _y
{-# INLINE _g #-}

-- | Cofactor of M511 curve.
_h :: Integer
_h = 0x8
{-# INLINE _h #-}

-- | Order of M511 curve.
_n :: Integer
_n = 0x100000000000000000000000000000000000000000000000000000000000000017b5feff30c7f5677ab2aeebd13779a2ac125042a6aa10bfa54c15bab76baf1b
{-# INLINE _n #-}

-- | Characteristic of M511 curve.
_p :: Integer
_p = 0x7fffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff45
{-# INLINE _p #-}

-- | Coordinate @X@ of M511 curve.
_x :: Fp
_x = 0x5
{-# INLINE _x #-}

-- | Coordinate @Y@ of M511 curve.
_y :: Fp
_y = 0x2fbdc0ad8530803d28fdbad354bb488d32399ac1cf8f6e01ee3f96389b90c809422b9429e8a43dbf49308ac4455940abe9f1dbca542093a895e30a64af056fa5
{-# INLINE _y #-}
