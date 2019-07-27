module Curve.Montgomery.M221
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

-- | M221 curve
data M221

-- | Field of M221 curve
type Fp = PrimeField 0x1ffffffffffffffffffffffffffffffffffffffffffffffffffffffd

-- | M221 curve is a Montgomery curve
instance MCurve M221 Fp where
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

-- | Point of M221 curve
type P = MPoint M221 Fp

-------------------------------------------------------------------------------
-- Parameters
-------------------------------------------------------------------------------

-- | Coefficient @A@ of M221 curve
_a :: Fp
_a = 0x1c93a
{-# INLINE _a #-}

-- | Coefficient @B@ of M221 curve
_b :: Fp
_b = 0x1
{-# INLINE _b #-}

-- | Generator of M221 curve
_g :: P
_g = A _x _y
{-# INLINE _g #-}

-- | Cofactor of M221 curve
_h :: Integer
_h = 0x8
{-# INLINE _h #-}

-- | Order of M221 curve
_n :: Integer
_n = 0x40000000000000000000000000015a08ed730e8a2f77f005042605b
{-# INLINE _n #-}

-- | Characteristic of M221 curve
_p :: Integer
_p = 0x1ffffffffffffffffffffffffffffffffffffffffffffffffffffffd
{-# INLINE _p #-}

-- | Coordinate @X@ of M221 curve
_x :: Fp
_x = 0x4
{-# INLINE _x #-}

-- | Coordinate @Y@ of M221 curve
_y :: Fp
_y = 0xf7acdd2a4939571d1cef14eca37c228e61dbff10707dc6c08c5056d
{-# INLINE _y #-}
