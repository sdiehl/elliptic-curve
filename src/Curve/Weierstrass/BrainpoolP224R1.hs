module Curve.Weierstrass.BrainpoolP224R1
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

-- | BrainpoolP224R1 curve.
data BrainpoolP224R1

-- | Field of points of BrainpoolP224R1 curve.
type Fq = PrimeField 0xd7c134aa264366862a18302575d1d787b09f075797da89f57ec8c0ff

-- | Field of coefficients of BrainpoolP224R1 curve.
type Fr = PrimeField 0xd7c134aa264366862a18302575d0fb98d116bc4b6ddebca3a5a7939f

-- | BrainpoolP224R1 curve is a Weierstrass curve.
instance WCurve BrainpoolP224R1 Fq where
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

-- | Point of BrainpoolP224R1 curve.
type P = WPoint BrainpoolP224R1 Fq

-------------------------------------------------------------------------------
-- Parameters
-------------------------------------------------------------------------------

-- | Coefficient @A@ of BrainpoolP224R1 curve.
_a :: Fq
_a = 0x68a5e62ca9ce6c1c299803a6c1530b514e182ad8b0042a59cad29f43
{-# INLINE _a #-}

-- | Coefficient @B@ of BrainpoolP224R1 curve.
_b :: Fq
_b = 0x2580f63ccfe44138870713b1a92369e33e2135d266dbb372386c400b
{-# INLINE _b #-}

-- | Generator of BrainpoolP224R1 curve.
_g :: P
_g = A _x _y
{-# INLINE _g #-}

-- | Cofactor of BrainpoolP224R1 curve.
_h :: Integer
_h = 0x1
{-# INLINE _h #-}

-- | Characteristic of BrainpoolP224R1 curve.
_q :: Integer
_q = 0xd7c134aa264366862a18302575d1d787b09f075797da89f57ec8c0ff
{-# INLINE _q #-}

-- | Order of BrainpoolP224R1 curve.
_r :: Integer
_r = 0xd7c134aa264366862a18302575d0fb98d116bc4b6ddebca3a5a7939f
{-# INLINE _r #-}

-- | Coordinate @X@ of BrainpoolP224R1 curve.
_x :: Fq
_x = 0xd9029ad2c7e5cf4340823b2a87dc68c9e4ce3174c1e6efdee12c07d
{-# INLINE _x #-}

-- | Coordinate @Y@ of BrainpoolP224R1 curve.
_y :: Fq
_y = 0x58aa56f772c0726f24c6b89e4ecdac24354b9e99caa3f6d3761402cd
{-# INLINE _y #-}
