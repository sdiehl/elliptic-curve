module Curve.Weierstrass.BN512
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

-- | BN512 curve.
data BN512

-- | Field of points of BN512 curve.
type Fq = PrimeField 0xfffffffffffffffffffffffffff9ec7f01c60ba1d8cb5307c0bbe3c111b0ef455146cf1eacbe98b8e48c65deab236fe1916a55ce5f4c6467b4eb280922adef33

-- | Field of coefficients of BN512 curve.
type Fr = PrimeField 0xfffffffffffffffffffffffffff9ec7f01c60ba1d8cb5307c0bbe3c111b0ef445146cf1eacbe98b8e48c65deab2679a34a10313e04f9a2b406a64a5f519a09ed

-- | BN512 curve is a Weierstrass curve.
instance WCurve BN512 Fq where
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

-- | Point of BN512 curve.
type P = WPoint BN512 Fq

-------------------------------------------------------------------------------
-- Parameters
-------------------------------------------------------------------------------

-- | Coefficient @A@ of BN512 curve.
_a :: Fq
_a = 0x0
{-# INLINE _a #-}

-- | Coefficient @B@ of BN512 curve.
_b :: Fq
_b = 0x3
{-# INLINE _b #-}

-- | Generator of BN512 curve.
_g :: P
_g = A _x _y
{-# INLINE _g #-}

-- | Cofactor of BN512 curve.
_h :: Integer
_h = 0x1
{-# INLINE _h #-}

-- | Characteristic of BN512 curve.
_q :: Integer
_q = 0xfffffffffffffffffffffffffff9ec7f01c60ba1d8cb5307c0bbe3c111b0ef455146cf1eacbe98b8e48c65deab236fe1916a55ce5f4c6467b4eb280922adef33
{-# INLINE _q #-}

-- | Order of BN512 curve.
_r :: Integer
_r = 0xfffffffffffffffffffffffffff9ec7f01c60ba1d8cb5307c0bbe3c111b0ef445146cf1eacbe98b8e48c65deab2679a34a10313e04f9a2b406a64a5f519a09ed
{-# INLINE _r #-}

-- | Coordinate @X@ of BN512 curve.
_x :: Fq
_x = 0x1
{-# INLINE _x #-}

-- | Coordinate @Y@ of BN512 curve.
_y :: Fq
_y = 0x2
{-# INLINE _y #-}
