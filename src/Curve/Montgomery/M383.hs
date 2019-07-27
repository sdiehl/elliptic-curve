module Curve.Montgomery.M383
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

import Curve.Montgomery (MCurve(..), MPoint, Point(..))

-------------------------------------------------------------------------------
-- Types
-------------------------------------------------------------------------------

-- | M383 curve
data M383

-- | Field of M383 curve
type Fp = PrimeField 0x7fffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff45

-- | M383 curve is a Montgomery curve
instance MCurve M383 Fp where
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

-- | Point of M383 curve
type P = MPoint M383 Fp

-------------------------------------------------------------------------------
-- Parameters
-------------------------------------------------------------------------------

-- | Coefficient @A@ of M383 curve
_a :: Fp
_a = 0x1f82fe
{-# INLINE _a #-}

-- | Coefficient @B@ of M383 curve
_b :: Fp
_b = 0x1
{-# INLINE _b #-}

-- | Generator of M383 curve
_g :: P
_g = A _x _y
{-# INLINE _g #-}

-- | Cofactor of M383 curve
_h :: Integer
_h = 0x8
{-# INLINE _h #-}

-- | Order of M383 curve
_n :: Integer
_n = 0x10000000000000000000000000000000000000000000000006c79673ac36ba6e7a32576f7b1b249e46bbc225be9071d7
{-# INLINE _n #-}

-- | Characteristic of M383 curve
_p :: Integer
_p = 0x7fffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff45
{-# INLINE _p #-}

-- | Coordinate @X@ of M383 curve
_x :: Fp
_x = 0xc
{-# INLINE _x #-}

-- | Coordinate @Y@ of M383 curve
_y :: Fp
_y = 0x1ec7ed04aaf834af310e304b2da0f328e7c165f0e8988abd3992861290f617aa1f1b2e7d0b6e332e969991b62555e77e
{-# INLINE _y #-}
